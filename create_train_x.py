import pandas as pd
import numpy as np
import pickle

# `jerseyNumber == jerseyNumber` and `yards_after_catch==yards_after_catch` remove NaN values
tracking = pd.concat([
    pd.read_parquet(f'data/processed_tracking_week_{week}.parquet')\
        .query('(pass_attempt==1)&(jerseyNumber==jerseyNumber)&(event==\'pass_outcome_caught\')&(yards_after_catch==yards_after_catch)') for week in range(1,10)])\
        .query('uniquePlayId!=\'2022110608_2351\'') # this play has two pass_outcome_caught events for whatever reason

features = ['gameId',
            'playId',
            'frameId',
            'isAugmented',
            'uniquePlayId',
            'uniqueFrameId',
            'isOnOffense', 
            'x', 
            'y', 
            'Sx',
            'Sy',
            'player_minus_carrier_x', 
            'player_minus_carrier_y', 
            'player_minus_carrier_Sx', 
            'player_minus_carrier_Sy', 
            'isBallcarrier']
df_all_feats = tracking[features]


grouped = df_all_feats.groupby(['gameId','playId','frameId','isAugmented'])

# Why is the training data an 11 x 10 x 10 tensor?
# 11 = number of defensive players
# 10 = number of offensive players EXCLUDING ball carrier
#      why not 11? because ballcarrier info already contained in data
# 10 = number of features contained in the model

train_x = np.zeros([len(grouped.size()),11,10,10])
i = 0
frame_ids = df_all_feats['uniqueFrameId'].drop_duplicates().values

for name, group in grouped:
    [[rusher_x, rusher_y, rusher_Sx, rusher_Sy]] = group.loc[group['isBallcarrier']==1,['x', 'y','Sx','Sy']].values

    offense_ids = group[group['isOnOffense'] & ~group['isBallcarrier']].index
    defense_ids = group[~group['isOnOffense']].index
    for j, defense_id in enumerate(defense_ids):
        [def_x, def_y, def_Sx, def_Sy] = group.loc[defense_id,['x', 'y','Sx','Sy']].values

        [def_rusher_x, def_rusher_y] = group.loc[defense_id,['player_minus_carrier_x', 'player_minus_carrier_y']].values
        [def_rusher_Sx, def_rusher_Sy] =  group.loc[defense_id,['player_minus_carrier_Sx', 'player_minus_carrier_Sy']].values

        train_x[i,j,:,:4] = group.loc[offense_ids,['Sx','Sy','x', 'y']].values - np.array([def_Sx, def_Sy, def_x,def_y])
        train_x[i,j,:,-6:] = [def_rusher_Sx, def_rusher_Sy, def_rusher_x, def_rusher_y, def_Sx, def_Sy]
    i+=1

pickle.dump(train_x, open("train_x.p", "wb"))