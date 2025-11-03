       IDENTIFICATION DIVISION.
       PROGRAM-ID.      BWVDEL.
       DATE-WRITTEN.
      *****************************************************
      *               VERIFY BORROWER DELETE
      *
      * REV:
      *  073097 BAH DISPLAY MORE SPECIFIC ERROR MESSAGES
      *  MG  990927  FIXED VDUSTATUS/IO-FG/STAT PER DEVELOPMENT MEETING.
      *  BAH 130201 A30 CONVERSION
      *  BAH 181126 GLOBAL LHFILE
      *  BAH 181203 GLOBAL LNFILE
      *  BAH 20220829 HARD CODED START
      *  BAH 20230712 COMMENT OUT 3RD AND 4TH BORROWER TESTS, LN6 AND LN7
      *****************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.

      *****************************************
       WORKING-STORAGE SECTION.
      *****************************************
       77  SCCS-IDENTIFIER PIC X(55) VALUE
           "@(#)CL/BWVDEL A30 02/01/13-13:33:15 09/18/14-13:46:49 >".
       COPY "LIBLP/LP01LN.CPY".
       COPY "LIBLP/LP01LN_SQL.CPY".
       COPY "LIBLP/LPWSLN.CPY".
       COPY "LIBLP/LP01LH.CPY".
       COPY "LIBLP/LP01LH_SQL.CPY".
       COPY "LIBLP/LPWSLH.CPY".

       01  ERRCD           PIC X     VALUE SPACES.

       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBGB/HELPLINK.CPY".

       COPY "LIBGB/GETFMW.CPY".
       COPY "LIBLP/LP01BW.CPY".
       01  LINK-IO-FG           PIC 9.
       PROCEDURE DIVISION
            USING FORM-PATHNAME BW-REC LINK-IO-FG EXIT-PATHNAME.

      ************************************
       MAIN-PROGRAM SECTION.
      ************************************
           PERFORM SQL-CONNECT.

      D    STOP MESS.
           MOVE FORM-PATHNAME TO FORM-PATH.
       MAIN-MODULE.
           MOVE "DELETE VERIFICATION IN PROGRESS........." TO WR-BUF.
           MOVE "WSEL:D." TO WR-LIST.
           PERFORM CALL-WRFORM.

           MOVE 9 TO LINK-IO-FG.

           PERFORM VERIFY-DELETE.

           GO TO END-ROUTINE.

      ***********************************
       VERIFY-DELETE SECTION.
      ***********************************
           IF BW-LOANS NOT = 0
              MOVE "LOAN COUNT NOT = 0" TO MESS
              PERFORM ERROR-MESSAGE.
           IF BW-LIASMLN NOT = 0
              MOVE "SMALL LOAN LIABILITY NOT = 0" TO MESS
              PERFORM ERROR-MESSAGE.
           IF BW-LIAOTH NOT = 0
              MOVE "OTHER LOAN LIABILITY NOT = 0" TO MESS
              PERFORM ERROR-MESSAGE.
           IF BW-INTLYR NOT = 0
              MOVE "INTEREST LAST YEAR NOT = 0" TO MESS
              PERFORM ERROR-MESSAGE.
           IF BW-INTYTD NOT = 0
              MOVE "INTEREST YTD NOT = 0" TO MESS
              PERFORM ERROR-MESSAGE.
           IF (BW-LASTPODATE NOT = 0)
              MOVE "LAST PAYOFF DATE NOT = 0" TO MESS
              PERFORM ERROR-MESSAGE.

           PERFORM CHECK-LOAN-ACTIVITY.
           IF IO-BAD
              MOVE "BORROWER HAS LOANS/LOAN HISTORY" TO MESS
              PERFORM ERROR-MESSAGE.

           GO TO EXIT-VERIFY-DELETE.

       ERROR-MESSAGE.
           PERFORM SEND-MESS.
           MOVE 9 TO IO-FG.

       EXIT-VERIFY-DELETE.
           EXIT.

      ****************************
       CHECK-LOAN-ACTIVITY SECTION.
      ****************************
           PERFORM OPEN-LN4-FILE.
           PERFORM OPEN-LH1-FILE.

           MOVE LN-PATH-OWNBR    TO LN-OWNBR
                                    QLN4-WBEG-OWNBR
                                    QLN4-WEND-OWNBR.
           MOVE BW-SSNO          TO LN-SSNO(1)
                                    QLN4-WBEG-SSNO1
                                    QLN4-WEND-SSNO1.

           PERFORM START-LN4-FILE.
           PERFORM READ-LN4-FILE-NEXT.
           IF (IO-FG NOT = 0) OR (LN-SSNO(1) NOT = BW-SSNO)
                               OR (LN-OWNBR NOT = LN-PATH-OWNBR)
              NEXT SENTENCE
           ELSE
              MOVE 9 TO IO-FG
              IF LN-POCD = "**"
                 MOVE "BORROWER HAS A VOIDED LOAN" TO MESS
                 PERFORM ERROR-MESSAGE
              END-IF
              GO TO EXIT-CHECK-LOAN-ACTIVITY.

           MOVE LN-PATH-OWNBR    TO LN-OWNBR
                                    QLN5-WBEG-OWNBR
                                    QLN5-WEND-OWNBR.
           MOVE BW-SSNO          TO LN-SSNO(2)
                                    QLN5-WBEG-SSNO2
                                    QLN5-WEND-SSNO2.
           PERFORM START-LN5-FILE.
           PERFORM READ-LN5-FILE-NEXT.
           IF (IO-FG NOT = 0) OR (LN-SSNO(2) NOT = BW-SSNO)
                               OR (LN-OWNBR NOT = LN-PATH-OWNBR)
              NEXT SENTENCE
           ELSE
              MOVE 9 TO IO-FG
              IF LN-POCD = "**"
                 MOVE "BORROWER HAS A VOIDED LOAN" TO MESS
                 PERFORM ERROR-MESSAGE
              END-IF
              GO TO EXIT-CHECK-LOAN-ACTIVITY.

      *    MOVE LN-PATH-OWNBR    TO LN-OWNBR
      *	                            QLN6-WBEG-OWNBR
      *                             QLN6-WEND-OWNBR.
      *    MOVE BW-SSNO          TO LN-SSNO(3)
      *                             QLN6-WBEG-SSNO3
      *                             QLN6-WEND-SSNO3.
      *    PERFORM START-LN6-FILE.
      *    PERFORM READ-LN6-FILE-NEXT.
      *    IF (IO-FG NOT = 0) OR (LN-SSNO(3) NOT = BW-SSNO)
      *                        OR (LN-OWNBR NOT = LN-PATH-OWNBR)
      *       NEXT SENTENCE
      *    ELSE
      *       MOVE 9 TO IO-FG
      *       IF LN-POCD = "**"
      *          MOVE "BORROWER HAS A VOIDED LOAN" TO MESS
      *          PERFORM ERROR-MESSAGE
      *       END-IF
      *       GO TO EXIT-CHECK-LOAN-ACTIVITY.

      *    MOVE LN-PATH-OWNBR    TO LN-OWNBR
      *                             QLN7-WBEG-OWNBR
      *                             QLN7-WEND-OWNBR.
      *    MOVE BW-SSNO          TO LN-SSNO(4)
      *                             QLN7-WBEG-SSNO4
      *                             QLN7-WEND-SSNO4.
      *    PERFORM START-LN7-FILE.
      *    PERFORM READ-LN7-FILE-NEXT.
      *    IF (IO-FG NOT = 0) OR (LN-SSNO(4) NOT = BW-SSNO)
      *                        OR (LN-OWNBR NOT = LN-PATH-OWNBR)
      *       NEXT SENTENCE
      *    ELSE
      *       MOVE 9 TO IO-FG
      *       IF LN-POCD = "**"
      *          MOVE "BORROWER HAS A VOIDED LOAN" TO MESS
      *          PERFORM ERROR-MESSAGE
      *       END-IF
      *       GO TO EXIT-CHECK-LOAN-ACTIVITY.

           MOVE LH-PATH-OWNBR TO LH-BRNO
                                 QLH1-WBEG-BRNO
                                 QLH1-WEND-BRNO.
           MOVE BW-SSNO       TO LH-SSNO
                                 QLH1-WBEG-SSNO
                                 QLH1-WEND-SSNO.
           MOVE 0             TO LH-ACCTNO
                                 QLH1-WBEG-ACCTNO.
           MOVE ALL "9"       TO QLH1-WEND-ACCTNO.

           PERFORM START-LH1-FILE.
           PERFORM READ-LH1-FILE-NEXT.
           IF (IO-BAD) OR (LH-SSNO NOT = BW-SSNO)
                        OR (LH-BRNO NOT = LH-PATH-OWNBR)
              MOVE 0 TO IO-FG
           ELSE
              MOVE 9 TO IO-FG
              MOVE "BORROWER HAS PURGED LOAN HISTORY" TO MESS
              PERFORM ERROR-MESSAGE.

       EXIT-CHECK-LOAN-ACTIVITY.
           MOVE IO-FG TO LINK-IO-FG.
           PERFORM CLOSE-LN4-FILE.
           PERFORM CLOSE-LH1-FILE.

       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/DATER.CPY".

      *****************************
       MISC-ROUTINES SECTION.
      *****************************
       FORM-RESET.
       SETUP-LINK-INFO.
       VDU-CONVERT-FIELD.

      *****************************
       IO-ROUTINE SECTION.
      *****************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
      *    PERFORM CLOSE-LN7-FILE.
      *    PERFORM CLOSE-LN6-FILE.
           PERFORM CLOSE-LN5-FILE.
           PERFORM CLOSE-LN4-FILE.
           PERFORM CLOSE-LH1-FILE.
       COPY "LIBLP/LPLNGS_SQL.CPY".
       COPY "LIBLP/LPLHGS_SQL.CPY".
       COPY "LIBLP/LPLN4RN.CPY".
       COPY "LIBLP/LPLN5RN.CPY".
      *COPY "LIBLP/LPLN6RN.CPY".
      *COPY "LIBLP/LPLN7RN.CPY".
       COPY "LIBLP/LPLH1RN.CPY".
       COPY "LIBGB/FERRORS.CPY".

      *****************************
       END-ROUTINE SECTION.
      *****************************
           PERFORM CLOSE-FILES.
       EXIT-PROG.
       STOP-RUN.
           EXIT PROGRAM.
