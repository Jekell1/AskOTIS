      * COPYMEMBER: LIBLP/LPDUEDA
      ******************************************************************
      *          DUE DATE CHANGE RESTRICTIONS
      *
      *    NAME: LPDUEDA
      *    DESC: ROUTINE DETERMINES IF A DUE DATE CHANGE
      *          IS ALLOWED ON AN ACCOUNT
      *
      *    IN  : DUEDA-TRANS-DATE    (TRANSACTION DATE)
      *    OUT : DUEDA-ALLOWED      N - DUE DATE CHANGE NOT ALLOWED
      *                             Y - ALLOW DUE DATE CHANGE
      *    COPY: LPDUEDAW
      * REV:
      *  JTG 111197 ADDED BR-DUEDATE-CHANGE-CD OPTION 'M' MERCURY
      ******************************************************************
       DUE-DATE-CHANGE-TEST SECTION.
           MOVE "Y" TO DUEDA-ALLOWED.

      *----------------------------------------------------------------
      *    MERCURY RESTRICTION:
      *----------------------------------------------------------------
           IF BR-DUEDATE-CHANGE-CD  = "M"
              MOVE DUEDA-TRANS-DATE TO AGEING-DATE
              PERFORM COMPUTE-CONTRACTUAL
              IF CONTRACTUAL > 0
                 MOVE "N" TO DUEDA-ALLOWED
              END-IF

           ELSE
      *----------------------------------------------------------------
      *    RELIANCE RESTRICTION:
      *----------------------------------------------------------------
           IF BR-DUEDATE-CHANGE-CD = "R"
              MOVE LN-1STPYDATE TO PDUE-1STPYDATE
              MOVE DUEDA-TRANS-DATE TO PDUE-TODAY-DATE
              PERFORM PAST-DUE-DAYS-CALCULATION
              IF PDUE-DAYS > 0
                 MOVE "N" TO DUEDA-ALLOWED
              ELSE
                 MOVE LN-1STPYDATE TO PDTH-DATE-WORK
                 MOVE "N" TO PDTH-DEF-FG
                             PDTH-ALDEL-FG
                 PERFORM PAID-THRU-CALCULATION
                 IF PDTH-DATE-FULL < LN-1STPYDATE
                    MOVE "N" TO DUEDA-ALLOWED.

       DUE-DATE-CHANGE-TEST-EXIT.
           EXIT.
