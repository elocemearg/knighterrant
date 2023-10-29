CFLAGS=-O3 -Wall
LDFLAGS=-lpthread

knighterrant: knighterrant.c
	$(CC) -DRESTRICT_START_AND_END=1 $(CFLAGS) $(LDFLAGS) -o $@ $^

ketest5: knighterrant.c
	$(CC) -DTOUR_TEST=1 -DBOARD_DIM=5 $(CFLAGS) $(LDFLAGS) -o $@ $^

ketest6: knighterrant.c
	$(CC) -DTOUR_TEST=1 -DBOARD_DIM=6 $(CFLAGS) $(LDFLAGS) -o $@ $^

knighterrant-any-start-and-end: knighterrant.c
	$(CC) -DRESTRICT_START_AND_END=0 $(CFLAGS) $(LDFLAGS) -o $@ $^
