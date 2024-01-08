import pandas as pd
import numpy as np
import pickle

# `jerseyNumber == jerseyNumber` and `yards_after_catch==yards_after_catch` remove NaN values
tracking = pd.concat([
    pd.read_parquet(f'data/processed_tracking_week_{week}.parquet')\
        .query('(pass_attempt==1)&(jerseyNumber==jerseyNumber)&(event==\'pass_outcome_caught\')&(yards_after_catch==yards_after_catch)') for week in range(1,10)])\
        .query('uniquePlayId!=\'2022110608_2351\'') # this play has two pass_outcome_caught events for whatever reason

train_x_file = open("train_x.p", "rb")
train_x = pickle.load(train_x_file)
train_x_file.close()

train_y = tracking.drop_duplicates(subset='uniqueFrameId')[['uniqueFrameId', 'yards_after_catch']].reset_index(drop=True).copy()

# following code adapted from 2020 BDB winning Zoo implementation ()
# this chunk just imports the necessary classes/functions from tensorflow and sklearn

import tensorflow as tf

from tensorflow import keras

from keras import Model

from keras.layers import (
    Conv1D, Conv2D, MaxPooling1D, MaxPooling2D, AvgPool1D, AvgPool2D, Reshape,
    Input, Activation, Dense, Add, Lambda, Dropout) # BatchNormalization and LayerNormalization removed

from keras.optimizers import Adam # https://stackoverflow.com/questions/62707558/importerror-cannot-import-name-adam-from-keras-optimizers
from keras import backend as K
from keras.callbacks import Callback, EarlyStopping

from sklearn.model_selection import train_test_split
from sklearn.model_selection import KFold

num_classes_y = 1

def get_conv_net(num_classes_y): # param of num_classes_y
    #_, x, y, z = train_x.shape 
    inputdense_players = Input(shape=(11,10,10), name = "playersfeatures_input") # this is the initial tensor -- 11x10x10
    
    X = Conv2D(128, kernel_size=(1,1), strides=(1,1), activation='relu')(inputdense_players)
    X = Conv2D(160, kernel_size=(1,1), strides=(1,1), activation='relu')(X)
    X = Conv2D(128, kernel_size=(1,1), strides=(1,1), activation='relu')(X)
    
    # The second block of convolutions learns the necessary information per defense player before the aggregation.
    # For this reason the pool_size should be (1, 10). If you want to learn per off player the pool_size must be 
    # (11, 1)

    ## ^ I recommend we continue learning per defensive player. It may be worth testing size=(11,1) going forward
    # however, in theory the convolution should produce the same features, so might not be worth computation time
    Xmax = MaxPooling2D(pool_size=(1,10))(X) # max pooling layer
    Xmax = Lambda(lambda x1 : x1*0.3)(Xmax)

    Xavg = AvgPool2D(pool_size=(1,10))(X) # mean (avg) pooling layer
    Xavg = Lambda(lambda x1 : x1*0.7)(Xavg)

    X = Add()([Xmax, Xavg])
    X = Lambda(lambda y : K.squeeze(y,2))(X)
    X = tf.keras.layers.BatchNormalization()(X) # normalization layer
    
    # these convolutions are the next step in dimensionality reduction (ReLu activation)
    X = Conv1D(160, kernel_size=1, strides=1, activation='relu')(X)
    X = tf.keras.layers.BatchNormalization()(X)
    X = Conv1D(96, kernel_size=1, strides=1, activation='relu')(X)
    X = tf.keras.layers.BatchNormalization()(X)
    X = Conv1D(96, kernel_size=1, strides=1, activation='relu')(X)
    X = tf.keras.layers.BatchNormalization()(X)
    
    # creating the max and average pooling layers (As with prev. dimensionality reduction)
    Xmax = MaxPooling1D(pool_size=11)(X)
    Xmax = Lambda(lambda x1 : x1*0.3)(Xmax)

    Xavg = AvgPool1D(pool_size=11)(X)
    Xavg = Lambda(lambda x1 : x1*0.7)(Xavg) # these rescaling coefficients (0.3, 0.7, etc) seem a bit arbitrary.
    # we may want to experiment with them in the future


    X = Add()([Xmax, Xavg])
    X = Lambda(lambda y : K.squeeze(y,1))(X)
    
    X = Dense(96, activation="relu")(X)
    X = tf.keras.layers.BatchNormalization()(X)

    # The below code implements the second-last layer of the NN via a linear reduction layer, Dense()
    # this is the last actual "neural network" layer before the linear output layer

    X = Dense(256, activation="relu")(X)
    X = tf.keras.layers.LayerNormalization()(X)
    X = Dropout(0.3)(X)

    # linear output layer using Dense()
    outlinear = Dense(num_classes_y, activation='linear', name = "output")(X)
    
    model = Model(inputs = [inputdense_players], outputs = outlinear)
    return model

# This chunk sets up the model evaluation function as the class Metric()
# this will be used in the cross-validation function in the below chunk

class Metric(Callback):
    def __init__(self, model, callbacks, data):
        super().__init__()
        self.model = model
        self.callbacks = callbacks
        self.data = data

    def on_train_begin(self, logs=None):
        for callback in self.callbacks:
            callback.on_train_begin(logs)

    def on_train_end(self, logs=None):
        for callback in self.callbacks:
            callback.on_train_end(logs)

    def on_epoch_end(self, batch, logs=None):
        X_valid, y_valid = self.data[0], self.data[1]

        y_pred = self.model.predict(X_valid)
        y_true = np.clip(np.cumsum(y_valid, axis=1), 0, 1)
        y_pred = np.clip(np.cumsum(y_pred, axis=1), 0, 1)
        val_s = ((y_true - y_pred) ** 2).sum(axis=1).sum(axis=0) / (199 * X_valid.shape[0])
        logs['val_CRPS'] = val_s
        
        for callback in self.callbacks:
            callback.on_epoch_end(batch, logs)

# This chunk also does a lot of heavy lifting -- the actual model optimization
models = []
kf = KFold(n_splits=20, shuffle=True)
score = []

for i, (tdx, vdx) in enumerate(kf.split(train_x, train_y)):
    X_train, X_val = train_x[tdx], train_x[vdx],
    y_train, y_val = train_y.iloc[tdx]['yards_after_catch'].values, train_y.iloc[vdx]['yards_after_catch'].values

# this code shouldn't actually be necessary, but I'm leaving it in to catch potential errors

    y_train_values = y_train
    y_val_values = y_val

    y_train_values = y_train.astype('float32')
    y_val_values = y_val.astype('float32')

    y_train_values = np.expand_dims(y_train_values, 1)
    y_val_values = np.expand_dims(y_val_values, 1)

    
    # fitting the model
    model = get_conv_net(num_classes_y)

# I'm relying on the previous "literature" (posts from the 2020 BDB) to justify this early stopping criterion.
# We can play with it in the future, but defaulting to the previous solution makes sense for now. 
    es = EarlyStopping(monitor='val_CRPS',
                        mode='min',
                        restore_best_weights=True,
                        verbose=0,
                        patience=10) # this is one param we would tweak
    
    es.set_model(model)
    metric = Metric(model, [es], [X_val, y_val_values])

    lr_i = 1e-3
    lr_f = 5e-4
    n_epochs = 30 # this is another param we would tweak

    decay = (1-lr_f/lr_i)/((lr_f/lr_i)* n_epochs - 1)  #Time-based decay formula
    alpha = (lr_i*(1+decay))

    
    opt = Adam(learning_rate=1e-3) # this is another param we would tweak
    model.compile(loss="mean_squared_error",
                  optimizer=opt)
    
    model.fit(X_train,
              y_train_values, 
              epochs=n_epochs,
              batch_size=64, # batch size is also adjustable, though 64 is a standard in CNN literature (not just BDB related, but generally)
              # however, no one really knows what an optim version of these params is (you're supposed to experiment w/ them)
              verbose=0,
              callbacks=[metric],
              validation_data=(X_val, y_val_values))

# choosing the optimal loss score in our model's performance...
    val_crps_score = min(model.history.history["val_CRPS"])
    print("Val loss: {}".format(val_crps_score))
    
    score.append(val_crps_score)

    models.append(model)

# output the mean score across the four CV subsets of the data
mean_score = np.mean(score)
model_file = open('model.sav', 'wb')
pickle.dump(model, model_file)
model_file.close()
pred_yac = model.predict(train_x).flatten()

frame_ids = tracking['uniqueFrameId'].unique()
tracking['xyac'] = tracking['uniqueFrameId'].map({frame_ids[i]:pred_yac[i] for i in range(len(frame_ids))})
for week in range(1,10):
    tracking.loc[tracking['week']==week].to_parquet(f'data/tracking_with_xyac_week_{week}.parquet')

