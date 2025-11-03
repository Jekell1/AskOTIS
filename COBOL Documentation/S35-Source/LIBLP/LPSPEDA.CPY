      * COPYMEMBER: LIBLP/LPSPEDA
      ***********************************************************************
      *
      *  NAME:  LPSPEDA
      *  DESC:  VALIDATES:
      *            INSURANCE PREMIUM FOR O1 & O2 ADDONS
      *
      *         N O T E:
      *                 IF THIS ROUTINE IS CHANGED ALSO CHANGE
      *                          [ LPSPEDL ]
      *                          [ LPSPEDP ]
      *
      *  IN  :
      *
      *  OUT :  VALID-LOAN (Y OR N)
      *
      * REV:
      *  JTG 000113 NO CHANGE REQUIRED (REVIEWED) ADDED SP-INSREJECT-FG
      *             OPTION 'C', WHICH WILL CAUSE WRITING NO OF MONTHS
      *             INSURANCE NOT IN VIOLATION, WISCONSIN #1032
      ***********************************************************************
       VERIFY-TERMCEIL SECTION.
           MOVE " " TO LIMIT-ERROR ST-FIRST-STEP MESS.
           MOVE "Y" TO VALID-LOAN.
           MOVE 0 TO SUB.
           PERFORM VERIFY-OT-INSURANCE.

       VERIFY-TERMCEIL-EXIT.
           IF VALID-LOAN = "N"
              MOVE "CANNOT UPDATE - CORRECT ENTRIES TO CONTINUE"
                TO MESS
              PERFORM SEND-MESS.

       VERIFY-OT-INSURANCE SECTION.
           COMPUTE N-WK1 ROUNDED = LN-CURBAL / 3.

           IF SP-INS-O1-EDIT = "A"
              IF PD-INSPREM(4) > N-WK1
                 MOVE "EXCEEDS MAXIMUM OF" TO LIMIT-TYPE-LIT
                 MOVE " " TO LIMIT-AMT-TYPE
                 STRING "O1-INS.PREMIUM" DELIMITED BY "  "
                    INTO LIMIT-AMT-TYPE
                 MOVE N-WK1 TO LIMIT-AMT
                 MOVE "N" TO VALID-LOAN
                 PERFORM DSPLY-BOUNDARY-ERRMSG.
           IF SP-INS-O2-EDIT = "A"
              IF PD-INSPREM(5) > N-WK1
                 MOVE "EXCEEDS MAXIMUM OF" TO LIMIT-TYPE-LIT
                 MOVE " " TO LIMIT-AMT-TYPE
                 STRING "O2-INS.PREMIUM" DELIMITED BY "  "
                    INTO LIMIT-AMT-TYPE
                 MOVE N-WK1 TO LIMIT-AMT
                 MOVE "N" TO VALID-LOAN
                 PERFORM DSPLY-BOUNDARY-ERRMSG.

       VERIFY-OT-INSURANCE-EXIT.
           EXIT.

       DSPLY-BOUNDARY-ERRMSG SECTION.
           UNSTRING LIMIT-AMT-LIT DELIMITED BY ALL " " INTO
              LIMIT-WORKER LIMIT-AMT-LIT.
           STRING LIMIT-AMT-TYPE, " ", LIMIT-TYPE-LIT, " "
              LIMIT-AMT-LIT, " ", "- CANNOT UPDATE."
              DELIMITED BY "  " INTO MESS.
           PERFORM SEND-MESS.
           MOVE " " TO LIMIT-AMT-LIT MESS.

