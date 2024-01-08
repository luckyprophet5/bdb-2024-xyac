import pandas as pd
import math
import numpy as np

# defining helpers:

# helper for standardizing direction
def process_play_direction(df):
    df['isPlayLeftToRight'] = df['playDirection'].apply(lambda val: True if val.strip() == 'right' else False)
    return df

def standarize_direction(df):
    # adjusted the data to always be from left to right
    df['homePossession'] = df['possessionTeam'] == df['homeTeamAbbr']

    df['dir'] = np.mod(90 - df["dir"], 360) * math.pi/180.0

    df['toLeft'] = df['playDirection'] == "left"
    df['teamOnOffense'] = "home"
    df.loc[df['possessionTeam'] != df['homeTeamAbbr'], 'teamOnOffense'] = "away"
    df['isOnOffense'] = df['club'] == df['possessionTeam'] # Is player on offense?
    df.loc[df['toLeft'], 'x'] = 120 - df.loc[df['toLeft'], 'x']
    df.loc[df['toLeft'], 'y'] = 160/3 - df.loc[df['toLeft'], 'y']
    df.loc[df['toLeft'], 'dir'] = np.mod(np.pi + df.loc[df['toLeft'], 'dir'], 2*np.pi)

    #Replace Null in Dir_rad
    df.loc[(df['isOnOffense']) & df['dir'].isna(),'dir'] = 0.0
    df.loc[~(df['isOnOffense']) & df['dir'].isna(),'dir'] = np.pi
    return df

# add mirrored plays to boost sample size
# (we ended up not augmenting since the dataset was so large as-is)
def data_augmentation(df, sample_ids):
    df_sample = df.loc[df['uniquePlayId'].isin(sample_ids)].copy()
    df_sample['y'] = 160/3  - df_sample['y']
    df_sample['dir'] = df_sample['dir'].apply(lambda x: 2*np.pi - x)
    df_sample['uniqueFrameId'] = df_sample['uniquePlayId'] + '_' + df_sample['frameId'].astype(str) + '_1'
    df_sample['uniquePlayId'] = df_sample['uniquePlayId'].apply(lambda x: str(x)+'_aug')
    df_sample['isAugmented'] = 1
    return df_sample

# add player speed and player location + velocity rel to ballcarrier
def add_features(df):
    # More feature engineering for all:
    df['Sx'] = df['s']*df['dir'].apply(math.cos)
    df['Sy'] = df['s']*df['dir'].apply(math.sin)

    df['isBallcarrier'] = df['nflId']==df['ballCarrierId']
    carriers = df[df['isBallcarrier']].copy()[['uniqueFrameId', 'x', 'y', 'Sx', 'Sy']].rename(columns={'x':'ballcarrier_x', 'y':'ballcarrier_y', 'Sx':'ballcarrier_Sx', 'Sy':'ballcarrier_Sy'})
    df = df.merge(carriers, on='uniqueFrameId', how='left')
    df['player_minus_carrier_x'] = df['ballcarrier_x'] - df['x']
    df['player_minus_carrier_y'] = df['ballcarrier_y'] - df['y']
    # Velocity parallel to direction of carrier:
    df['player_minus_carrier_Sx'] = df['ballcarrier_Sx'] - df['Sx']
    df['player_minus_carrier_Sy'] = df['ballcarrier_Sy'] - df['Sy']
    return df

def process_data(tracking):
    df = process_play_direction(tracking)
    df = standarize_direction(df)
    # sample_ids = np.random.choice(df['uniquePlayId'].unique(), int(0.5*len(df['uniquePlayId'].unique())))
    # df_players_aug = data_augmentation(df, sample_ids)
    # df = pd.concat([df, df_players_aug])
    df = df.reset_index()
    # This is necessary to maintain the order when in the next cell we use groupby
    df.sort_values(by=['gameId','playId','frameId','isAugmented'],inplace=True)
    df = add_features(df)
    return df

columns = ['gameId','playId','isAugmented','frameId','uniquePlayId','uniqueFrameId','club', 'nflId', 'isOnOffense', 'jerseyNumber', 'event', 'x', 'y', 's', 'a', 'dis' ,'o', 'dir', 'Sx', 'Sy', 'player_minus_carrier_x', 'player_minus_carrier_y', 'player_minus_carrier_Sx', 'player_minus_carrier_Sy', 'epa', 'isBallcarrier']

plays = pd.read_csv("data/plays.csv")
games = pd.read_csv("data/games.csv")
plays = plays.merge(games[["week", "gameId", "homeTeamAbbr", "visitorTeamAbbr"]], on=["gameId"], how="left")

pbp = pd.read_parquet('data/play_by_play_2022.parquet')
pbp['uniquePlayId'] = pbp['old_game_id'].astype(str) + '_' + pbp['play_id'].astype(int).astype(str)

for week in range(1,10):
    tracking_ = pd.read_csv(f"data/tracking_week_{week}.csv")
    tracking = tracking_.merge(plays[["week", "gameId", "playId", "ballCarrierId", "possessionTeam", "homeTeamAbbr", "visitorTeamAbbr"]], on=["gameId", "playId"], how="left")

    tracking['uniquePlayId'] = tracking['gameId'].astype(str) + '_' + tracking['playId'].astype(str)
    tracking['uniqueFrameId'] = tracking['uniquePlayId'] + '_' + tracking['frameId'].astype(str) + '_0' # _0 at end denotes non-augmented play
    tracking['isAugmented'] = 0

    tracking = tracking.merge(pbp[['uniquePlayId','epa','yards_after_catch','pass_attempt']], on='uniquePlayId', how='left')
    
    tracking = process_data(tracking)
    tracking.to_parquet(f'data/processed_tracking_week_{week}.parquet')
    
    del(tracking)
    del(tracking_)
    print(f"processed week {week}")