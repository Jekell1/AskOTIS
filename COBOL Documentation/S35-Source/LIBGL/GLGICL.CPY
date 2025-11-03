      * COPYMEMBER: LIBGL/GLGICL
      ******************************************************************
      *
      *  IDENTIFICATION: LIBGL/GLGICL
      *
      *  DESCRIPTION   : GLOBAL INTERFACE FILE CLEAR ROUTINE
      *
      *=================================================================
      * REV:
      * BAH 1998.0629 RENAMED GI-EREXTCHG TO GI-ERADDON AND
      *               CHANGED GI-UNEXTCHG TO GI-UNADDON
      * KEC 1999.0520 FIXED THE ROUTINE; WAS CLEARING TWICE.
      * KEC 2017.0215 {PR#00000} PARADATA <A30> [JAY CISZEWSKI]
      *          FIXED ROUTINE; WAS NOT USED ANYWHERE;
      *          CLEAR WAS BEING DONE MN/GIMAN1;
      *          MOVE ALL NINES TO GI-LN-APPROVAL-LIMIT.
      *  CS 190514 CLEAR FUTURE FIELDS
      *  BAH 20210427 REMOVED FUTURE FIELDS
      ******************************************************************
       CLEAR-GI-FILE SECTION.

           MOVE SPACES TO GI-REC.
           MOVE ZEROES TO GI-BRANCH
                          GI-CLASS.

           MOVE ZEROS  TO GI-LTOUCH-DATE.

           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > GI-ACCT-IX-MAX
                   MOVE ZEROES TO GI-BR-ACCOUNT(SUB)
           END-PERFORM.

           MOVE 9999999 TO GI-LN-APPROVAL-LIMIT.

       CLEAR-GI-FILE-EXIT.
           EXIT.
