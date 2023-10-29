CFLAGS=-O3 -Wall
LDFLAGS=-lpthread
CSOURCES=knighterrant.c multithreadjobqueue.c
CHEADERS=knighterrant.h

knighterrant: $(CSOURCES) $(CHEADERS)
	$(CC) -DRESTRICT_START_AND_END=1 $(CFLAGS) $(LDFLAGS) -o $@ $(CSOURCES)

ketest5: $(CSOURCES) $(CHEADERS)
	$(CC) -DTOUR_TEST=1 -DBOARD_DIM=5 $(CFLAGS) $(LDFLAGS) -o $@ $(CSOURCES)

ketest6: $(CSOURCES) $(CHEADERS)
	$(CC) -DTOUR_TEST=1 -DBOARD_DIM=6 $(CFLAGS) $(LDFLAGS) -o $@ $(CSOURCES)

knighterrant-any-start-and-end: $(CSOURCES) $(CHEADERS)
	$(CC) -DRESTRICT_START_AND_END=0 $(CFLAGS) $(LDFLAGS) -o $@ $(CSOURCES)
