       IDENTIFICATION DIVISION.
       PROGRAM-ID.      RENEWA.
       DATE-WRITTEN.    01/18/2011.
      ************************************************************
      *                 (CL)
      *  DESCRIPTION:   FIND ALL PAIDOUT LOANS WITH PASSED SSNO
      *                 WORLD, IL
      *
      *                 CAN NOT MAKE A NEW LOAN ON SAME DAY AS 
      *                 PAYING OFF A LOAN
      *
      *                 MUST CHECK ALL 4 SLOTS
      *                 CALLED FROM LNVERI
      *
      * REV:
      *  110118 BAH NEW FOR WORLD, IL PR# 734
      *  BAH 181203 GLOBAL LNFILE
      *  BAH 20220830 HARD CODED START
      *  BAH 20230711 COMMENT OUT 3RD AND 4TH BORROWER TESTS, LN6 AND LN7
      ************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.

       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(55) VALUE
           "@(#)CL/RENEWA A30 04/18/13-14:45:51 09/18/14-13:47:09 >".
      **********************************
      *       F I L E   I F N          *
      **********************************
       COPY "LIBLP/LP01LN.CPY".
       COPY "LIBLP/LP01LN_SQL.CPY".
       COPY "LIBLP/LPWSLN.CPY".

       01  ERRCD           PIC X         VALUE SPACES.
       01  MSEL-LIST       PIC X(5)      VALUE "MSEL.".

       01  OLD-LN-SUB      PIC 9 VALUE 0.
       01  NEW-BW-SUB      PIC 9 VALUE 0.

       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".

      ****************************************
      *     LINKAGE PROGRAM SECTION:
      ****************************************
       COPY "LIBGB/GETFMW.CPY".
       COPY "LIBCL/RENEWAW.CPY".
       PROCEDURE DIVISION
             USING FORM-PATHNAME RENEWA-WORKERS EXIT-PATHNAME.

      ************************************************************
       MAIN-PROGRAM SECTION.
      ************************************************************

           PERFORM SQL-CONNECT.

           MOVE FORM-PATHNAME TO FORM-PATH.

           MOVE "SEARCHING FOR PAID OUT LOANS ....." TO MESS.
           PERFORM SEND-LEGEND.

           MOVE SPACES TO RENEWA-STOP-LOAN.

      D    STOP MESS.
      * TEST ALL 4 NEW LOAN BORROWERS IN ALL 4 OLD LOAN SLOTS:
      * TEST 2 NEW LOAN BORROWERS IN 2 OLD LOAN SLOTS:
           MOVE 1 TO OLD-LN-SUB.
           PERFORM TEST-SSNO.
           IF RENEWA-STOP-LOAN = "Y"
              GO TO END-ROUTINE.

      * TEST 2ND MAKER:
           MOVE 2 TO OLD-LN-SUB.
           PERFORM TEST-SSNO.
           IF RENEWA-STOP-LOAN = "Y"
              GO TO END-ROUTINE.

      *    MOVE 3 TO OLD-LN-SUB.
      *    PERFORM TEST-SSNO.
      *    IF RENEWA-STOP-LOAN = "Y"
      *       GO TO END-ROUTINE.

      *    MOVE 4 TO OLD-LN-SUB.
      *    PERFORM TEST-SSNO.

           GO TO END-ROUTINE.

      ************************************************************
       TEST-SSNO SECTION.
      ************************************************************
           PERFORM OPEN-LN4-FILE.

           MOVE 1 TO NEW-BW-SUB.

       TEST-NEXT-NEW.
           MOVE LN-PATH-OWNBR           TO LN-OWNBR.
           MOVE RENEWA-SSNO(NEW-BW-SUB) TO LN-SSNO(OLD-LN-SUB).

           PERFORM START-LN-FILE.
       READ-NEXT.
           PERFORM READ-LN-FILE-NEXT.
           IF IO-BAD OR (LN-OWNBR NOT = LN-PATH-OWNBR)
              GO TO TEST-SSNO-EXIT.

           IF LN-SSNO(OLD-LN-SUB) NOT = RENEWA-SSNO(NEW-BW-SUB)
              GO TO TEST-SSNO-EXIT.

           IF ( LN-POCD-NONLOAN )
              GO TO READ-NEXT.

           IF LN-PO-TRCD NOT = "PO"
              GO TO READ-NEXT.

      * CAN NOT MAKE A NEW LOAN ON SAME DAY AS A PAYOFF FOR SAME BORROWER
           IF LN-POFFDATE = RENEWA-LOANDATE
              MOVE "Y" TO RENEWA-STOP-LOAN
              GO TO TEST-SSNO-EXIT
           ELSE
              GO TO READ-NEXT.

       TEST-SSNO-EXIT.
      *     IF NEW-BW-SUB < 4
            IF NEW-BW-SUB < 2
               ADD 1 TO NEW-BW-SUB
               IF RENEWA-SSNO(NEW-BW-SUB) NOT = 0
                  GO TO TEST-NEXT-NEW.

            PERFORM CLOSE-LN4-FILE.

      ************************************************************
       START-LN-FILE SECTION.
      ************************************************************
           IF OLD-LN-SUB = 1
              MOVE LN-PATH-OWNBR TO QLN4-WBEG-OWNBR
                                    QLN4-WEND-OWNBR
              MOVE LN-SSNO(1)    TO QLN4-WBEG-SSNO1
                                    QLN4-WEND-SSNO1
              PERFORM START-LN4-FILE
           ELSE
              IF OLD-LN-SUB = 2
                 MOVE LN-PATH-OWNBR TO QLN5-WBEG-OWNBR
                                       QLN5-WEND-OWNBR
                 MOVE LN-SSNO(2)    TO QLN5-WBEG-SSNO2
                                       QLN5-WEND-SSNO2
                 PERFORM START-LN5-FILE.
      *       ELSE
      *          IF OLD-LN-SUB = 3
      *             MOVE LN-PATH-OWNBR TO QLN6-WBEG-OWNBR
      *                                   QLN6-WEND-OWNBR
      *             MOVE LN-SSNO(3)    TO QLN6-WBEG-SSNO3
      *                                   QLN6-WEND-SSNO3
      *             PERFORM START-LN6-FILE
      *          ELSE
      *             IF OLD-LN-SUB = 4
      *                MOVE LN-PATH-OWNBR TO QLN7-WBEG-OWNBR
      *                                      QLN7-WEND-OWNBR
      *                MOVE LN-SSNO(4)    TO QLN7-WBEG-SSNO4
      *                                      QLN7-WEND-SSNO4
      *                PERFORM START-LN7-FILE.

      ************************************************************
       READ-LN-FILE-NEXT SECTION.
      ************************************************************
           IF OLD-LN-SUB = 1
              PERFORM READ-LN4-FILE-NEXT
           ELSE
              IF OLD-LN-SUB = 2
                 PERFORM READ-LN5-FILE-NEXT.
      *       ELSE
      *          IF OLD-LN-SUB = 3
      *             PERFORM READ-LN6-FILE-NEXT
      *          ELSE
      *             IF OLD-LN-SUB = 4
      *                PERFORM READ-LN7-FILE-NEXT.


       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/SCREEN.CPY".

      ************************************************************
       MISC-ROUTINES SECTION.
      ************************************************************
       FORM-RESET.
       SETUP-LINK-INFO.
       VDU-CONVERT-FIELD.

       SEND-LEGEND.
           MOVE MESS TO WR-BUF.
           MOVE MSEL-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      ************************************************************
       IO-ROUTINE SECTION.
      ************************************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
      *    PERFORM CLOSE-LN7-FILE.
      *    PERFORM CLOSE-LN6-FILE.
           PERFORM CLOSE-LN5-FILE.
           PERFORM CLOSE-LN4-FILE.
       COPY "LIBLP/LPLNGS_SQL.CPY".
       COPY "LIBLP/LPLN4RN.CPY".
       COPY "LIBLP/LPLN5RN.CPY".
      *COPY "LIBLP/LPLN6RN.CPY".
      *COPY "LIBLP/LPLN7RN.CPY".

       COPY "LIBGB/FERRORS.CPY".

      ************************************************************
       END-ROUTINE SECTION.
      ************************************************************
           PERFORM CLOSE-FILES.
       EXIT-PROG.
       STOP-RUN.
           EXIT PROGRAM.
