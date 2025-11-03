      * COPYMEMBER: LIBLP/LPLTICL
      **************************************************************************
      *    LPLTICL    INSURANCE ADDON TRAILER FILE CLEAR ROUTINE
      *
      * REV:
      *  06/19/2015 BAH NEW
      * AMM 20210429 REMOVED FUTURE FIELDS
      **************************************************************************
       CLEAR-LTI-FILE SECTION.
           MOVE SPACES TO LTI-REC.

           MOVE 0 TO LTI-LTOUCH-DATE.

           PERFORM VARYING LTISUB FROM 1 BY 1 UNTIL LTISUB > 3
              MOVE 0 TO LTI-ANTICERN(LTISUB)
                        LTI-ANTICADJ(LTISUB)
                        LTI-UFERN(LTISUB)
                        LTI-ACCERN(LTISUB).

           MOVE 0 TO LTI-INS-TYPE LTI-ORIG-1STPYDATE LTI-PRIOR-NODEF
                     LTI-INSFEE(1) LTI-INSFEE(2)
                     LTI-INSCOMP LTI-INSEFF-DATE LTI-INSEXP-DATE
                     LTI-INSPREM LTI-INSCOVR LTI-INSCOMM
                     LTI-ANTICTERM LTI-INS-PAYMNT-INCR
                     LTI-INS-TOT-ADDON LTI-INT-LAST-EARNED
                     LTI-RB-WORKER LTI-ANTIC-START-DATE.

