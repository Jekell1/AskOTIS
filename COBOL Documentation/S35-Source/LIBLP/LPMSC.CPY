      * COPYMEMBER: LIBLP/LPMSC
      ******************************************************************
      *
      * IDENTIFICATION: LIBLP/LPMSC
      *
      * DESCRIPTION   : ROUTINES FOR FORM PRINTING
      * USED          : LONPMC, LONPS3 DOC DESIGN STUFF
      *
      *=================================================================
      * REV:
      * KEC 2016.0115 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         FIXED KEYS & RECS FOR FD-REC.
      *         ADDED NEW FDB-FILE; WAS IN FD-FILE.
      *
      ******************************************************************
      *
      *    G E T - D A T A B A S E
      *
      *=================================================================
      *    IN : DATA-FIELD                               [LIBLP/LPEXTW]
      *         IO-FG                                    [LIBGB/FILEWK]
      *         LAST-FDB1-KEY
      *
      *    OUT: D-B-SUB
      *         FB-REC                                   [LIBLP/LP01FB]
      *         IO-FG
      *         LAST-FB1-KEY
      ******************************************************************
       GET-DATABASE SECTION.

           MOVE 0 TO IO-FG.
           COMPUTE FDB-DBNO = (DATA-FIELD - 1) / 200 + 1.
           COMPUTE D-B-SUB = DATA-FIELD - (FDB-DBNO - 1) * 200.

           IF LAST-FDB1-KEY NOT = FDB1-KEY
              PERFORM READ-FDB1-FILE.

           IF IO-FG = 0
              IF FDB-DNAME(D-B-SUB) = " "
                 MOVE 9 TO IO-FG.

           MOVE FDB1-KEY TO LAST-FDB1-KEY.

       GET-DATABASE-EXIT.
           EXIT.

      ******************************************************************
      *
      *    A L P H A - S H I F T
      *
      *=================================================================
      *    IN : SFT WORKERS
      *         EFT WORKERS                              [LIBLP/LPEDTW]
      *
      *    OUT: EFT WORKERS
      ******************************************************************
       ALPHA-SHIFT SECTION.
       SFT-COUNT-SPACES.
           MOVE 0 TO SFT-CT SFT-SUB.
       SFT-LT-CHECK.
           ADD 1 TO SFT-SUB.
           IF SFT-SUB > 80
              GO TO ALPHA-SHIFT-EXIT.
           IF EDT-CHAR(SFT-SUB) = " "
              ADD 1 TO SFT-CT.
           IF EDT-CHAR(SFT-SUB) = "<"
              ADD 1 TO SFT-CT
              GO TO SFT-GT-CHECK.
           IF EDT-CHAR(SFT-SUB) NOT = " "
              MOVE 0 TO SFT-CT.
           GO TO SFT-LT-CHECK.
       SFT-GT-CHECK.
           ADD 1 TO SFT-SUB.
           IF SFT-SUB > 80
              GO TO ALPHA-SHIFT-EXIT.
           SUBTRACT SFT-CT FROM SFT-SUB GIVING SFT-SUB1.
           MOVE EDT-CHAR(SFT-SUB) TO EDT-CHAR(SFT-SUB1).
           IF SFT-SUB = 80
              PERFORM SFT-CLEAR UNTIL SFT-CT = 0
              GO TO ALPHA-SHIFT-EXIT.
           IF EDT-CHAR(SFT-SUB) = ">"
              MOVE " " TO EDT-CHAR(SFT-SUB)
              PERFORM SFT-CLEAR UNTIL SFT-CT = 0
              MOVE 1 TO SFT-CT
              GO TO SFT-LT-CHECK.
           IF EDT-CHAR(SFT-SUB) = "<"
              PERFORM SFT-CLEAR UNTIL SFT-CT = 0
              GO TO SFT-COUNT-SPACES.
           GO TO SFT-GT-CHECK.
       SFT-CLEAR.
           SUBTRACT SFT-CT FROM SFT-SUB GIVING SFT-SUB1.
           MOVE " " TO EDT-CHAR(SFT-SUB1).
           SUBTRACT 1 FROM SFT-CT.

       ALPHA-SHIFT-EXIT.
           EXIT.
