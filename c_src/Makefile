all: ../priv/rockse

ERL_BASE_PATH=/Users/teeteoh/Project/r171/lib/erl_interface-3.7.17
ERL_LIB_PATH=$(ERL_BASE_PATH)/lib
ERL_INCLUDE=$(ERL_BASE_PATH)/include 
ERL_LIB=-lerl_interface -lei

ROCKSE_BASE_PATH=/Users/teeteoh/Project/forum/rocksdb
ROCKSE_LIB=-lrocksdb
ROCKSE_LIB_PATH=$(ROCKSE_BASE_PATH)
ROCKSE_INCLUDE=$(ROCKSE_BASE_PATH)/include

INCLUDE=-I $(ROCKSE_INCLUDE) -I $(ERL_INCLUDE)
LIB=-L$(ROCKSE_LIB_PATH) -L$(ERL_LIB_PATH) $(ERL_LIB) $(ROCKSE_LIB)

../priv/rockse: rockse.c
	gcc -o ../priv/rockse rockse.c $(INCLUDE) $(LIB)

clean:
	- echo "Cleanning c_src...."
