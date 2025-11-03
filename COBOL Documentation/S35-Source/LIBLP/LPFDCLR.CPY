      * COPYMEMBER: LIBLP/LPFDCLR
      ******************************************************************
      *
      *    LPFDCL   FORM DESCRIPTOR FILE CLEAR ROUTINE
      *
      *=================================================================
      * REV:
      * KEC 2016.0118 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         FIXED KEYS & RECS FOR FD-REC.
      *         ADDED NEW FDB-FILE; WAS IN FD-FILE.
      *  CS 190514 ADD FUTURE FIELDS
      *  AMM 2021.0426  REMOVED FUTURE FIELDS
      ******************************************************************
       CLEAR-FD-FILE SECTION.

           MOVE SPACES                     TO FD-REC
           MOVE ZEROES                     TO FD-FMNO.

           MOVE ZEROES                     TO FD-BODY FD-TOP FD-BOTTOM.

           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 50
                   MOVE ZEROES             TO FD-FIELDNO (SUB)
                                              FD-FIELDROW(SUB)
                                              FD-FIELDCOL(SUB)
                                              FD-FIELDED (SUB)
                                              FD-FIELDKEY(SUB)
           END-PERFORM.

           MOVE ZEROES                     TO FD-1STFILE
                                              FD-NEXTFM
                                              FD-MIN-FORM-SENT-DAYS.

       CLEAR-FD-FILE-EXIT.
           EXIT.

