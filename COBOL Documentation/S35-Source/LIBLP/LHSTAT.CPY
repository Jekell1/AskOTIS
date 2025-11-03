      * COPYMEMBER: LIBLP/LHSTAT
      ******************************************************************
      *   NAME:  LHSTAT 05/23/88
      *   DESC:  SETUP OF LOAN NUMBER FOR DISPLAY OR PRINT
      *          INSERT LPS-STATUSFLAGS INTO POSITIONS
      *          10 - 16 OF DISPLAY AREA.
      *   OUT :  LHS-DISPLAY-LNNO.
      *=================================================================
      * REV:
      * KEC 2015.0605 {PR#00000} PARADATA <A30> [JAY CISZEWSKI]
      *         REMOVED THE USE OF 'INDEXED BY' ON OCCURS
      *         FOR USE WITH MICROFOCUS.
      *
      ******************************************************************
       LHSTAT-DISPLAY SECTION.
       LHSTAT.
           MOVE SPACES TO LHS-DISPLAY-LNNO.
           MOVE LH-CLASS TO LHS-CLASS.
           MOVE "-" TO LHS-DASH.
           MOVE LH-ACCTNO TO LHS-NUMBER.
           MOVE LH-STATUS TO LHS-STATFLAGS.
       LHSTAT-CONT.
           IF BR-LOAN-NUM-FLAG NOT = "Y"
              GO TO LHSTAT-LAW-CODE.
           IF LHS-STATFG (1) = " "
              GO TO END-LHSTAT.
           IF LHS-STATFG (7) = " "
              MOVE "-"            TO LHS-STAT (1)
              MOVE LHS-STATFG (1) TO LHS-STAT (2)
              MOVE LHS-STATFG (2) TO LHS-STAT (3)
              MOVE LHS-STATFG (3) TO LHS-STAT (4)
              MOVE LHS-STATFG (4) TO LHS-STAT (5)
              MOVE LHS-STATFG (5) TO LHS-STAT (6)
              MOVE LHS-STATFG (6) TO LHS-STAT (7)
              GO TO END-LHSTAT.

           MOVE STAT---BAD                 TO STAT.

           PERFORM VARYING LHS-INDXH FROM 8 BY 1
                     UNTIL LHS-INDXH > 10
                        OR STAT-GOOD
                   IF ( LHS-STATFG (LHS-INDXH) = " " )
                      MOVE STAT---GOOD     TO STAT
                   END-IF
           END-PERFORM.

       LHSTAT-MOVES-A.

           SUBTRACT 07 FROM LHS-INDXH.
           MOVE LHS-STATFG (LHS-INDXH) TO LHS-STAT (1).
           ADD  01       TO LHS-INDXH.
           MOVE LHS-STATFG (LHS-INDXH) TO LHS-STAT (2).
           ADD  01      TO LHS-INDXH.
           MOVE LHS-STATFG (LHS-INDXH) TO LHS-STAT (3).
           ADD  01       TO LHS-INDXH.
           MOVE LHS-STATFG (LHS-INDXH) TO LHS-STAT (4).
           ADD  01       TO LHS-INDXH.
           MOVE LHS-STATFG (LHS-INDXH) TO LHS-STAT (5).
           ADD  01       TO LHS-INDXH.
           MOVE LHS-STATFG (LHS-INDXH) TO LHS-STAT (6).
           ADD  01       TO LHS-INDXH.
           MOVE LHS-STATFG (LHS-INDXH) TO LHS-STAT (7).
           GO TO END-LHSTAT.

       LHSTAT-LAW-CODE.
           MOVE LH-LAWCD    TO LH-LAWCODE-TEMP.
           MOVE "-"         TO LHS-STAT (1).
           MOVE LH-LAWCODE-TA TO LHS-STAT (2).
           MOVE LH-LAWCODE-TB TO LHS-STAT (3).

           IF ( LHS-STATFG (5) = SPACES )
              MOVE 05 TO LHS-INDXH
              GO TO LHSTAT-MOVES-B.

           MOVE STAT---BAD                 TO STAT.

           PERFORM VARYING LHS-INDXH FROM 1 BY 1
                     UNTIL LHS-INDXH > 10
                        OR STAT-GOOD
                   IF ( LHS-STATFG (LHS-INDXH) = SPACES )
                      MOVE STAT---GOOD     TO STAT
                   END-IF
           END-PERFORM.

       LHSTAT-MOVES-B.

           SUBTRACT 04 FROM LHS-INDXH.
           MOVE LHS-STATFG (LHS-INDXH) TO LHS-STAT (4).
           ADD  01       TO LHS-INDXH.
           MOVE LHS-STATFG (LHS-INDXH) TO LHS-STAT (5).
           ADD  01       TO LHS-INDXH.
           MOVE LHS-STATFG (LHS-INDXH) TO LHS-STAT (6).
           ADD  01       TO LHS-INDXH.
           MOVE LHS-STATFG (LHS-INDXH) TO LHS-STAT (7).
           ADD  01       TO LHS-INDXH.

       END-LHSTAT.
