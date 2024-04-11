SRC         = quagnes.cpp
OBJECTS     = $(SOURCE:.cpp=.o)
CC          = g++
INCLUDES    = -Iinc

#CFLAGS = -g $(INCLUDES)
CFLAGS = -Wall -Wpedantic -Wextra -O2 $(INCLUDES)

all: clean quagnes

quagnes: quagnes.o Agnes.o AgnesState.o 
	$(CC) $(CFLAGS) $< Agnes.o AgnesState.o -o $@

%.o: %.cpp
	$(CC) $(CFLAGS) -c $< -o $@

depend dep:
	$(CC) $(CFLAGS) -M $(SRC) > .dependencies

clean:
	rm -rf *.o

veryclean: clean
	rm -rf *.o *.exe
