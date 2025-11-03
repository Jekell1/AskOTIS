       IDENTIFICATION DIVISION.
       PROGRAM-ID.      NCINSA.
       DATE-WRITTEN.    112392.
      ************************************************************
      *                 (CL)
      *  DESCRIPTION:   DIRECT LOANS (NON SALES FINANCE)
      *                 NORTH CAROLINA RESTRICTION TEST FOR
      *                       INSURANCE ORGINATION FEES
      *
      *                 CALLED FROM XONPC0.C
      *
      *                 LOU WAGNER INTERPRETATION
      ************************************************************
      *  NOTE:
      *       MUST BE LOOKED AT FOR ADDON LOANS....
      *       LNAMT IS MOVED TO LE-REC
      ************************************************************
      *  REV:
      *  KEC 111496 CHANGED LN-POCD FOR RESCISSION.
      *  081197 BAH REMOVED ANY REFERENCE TO DUAL LOANS FROM HERE
      *             AND MADE A SEPARATE PROGRAM (CL/SPDUAL) WITH
      *             AN SPR FLAG....
      *  042798  CS ADD LP-TRCD RB AND RO TO RENEWAL TEST OF
      *             PB RN
      *  MG  990929 FIXED VDUSTATUS/IO-FG/STAT PER DEVELOPMENT MEETING.
      *  KEC 2015.0603 REMOVED THE USE OF 'INDEXED BY' ON OCCURS
      *                WITHIN LIBCL/NCINSW FOR USE WITH MICROFOCUS.
      *                ADDED 'NCINS-' IN FRONT OF PREVIOUS INDEXES.
      * BAH 2017.0405 ACTIVATE 8 DIGIT DATES, PD0006
      * KEC 2017.0425 REMOVED 'LE' FILE SINCE NOT USED.
      * BAH 2018.1203 GLOBAL LNFILE
      * BAH 2022.0826 HARD CODED START
      * BAH 2023.0711 COMMENT OUT 3RD AND 4TH BORROWER TESTS, LN6 AND LN7
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
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)CL/NCINSA.CBL S35 04/01/24-10:13:16 >".
       COPY "LIBLP/LP01LN.CPY".
       COPY "LIBLP/LP01LN_SQL.CPY".
       COPY "LIBLP/LPWSLN.CPY".
       COPY "LIBLP/LPWSLE.CPY".

       01  ERRCD            PIC X         VALUE " ".
       01  MSEL-LIST        PIC X(5)      VALUE "MSEL.".

       01  FSUB             PIC 9(3) COMP-3 VALUE 0.
       01  SSUB             PIC 9(3) COMP-3 VALUE 0.

       01  NC-INS-BAD-FEE-MESSAGE.
           03  FILLER            PIC X(15)   VALUE
           "F6, FEE CODE ( ".
           03  NC-INS-BAD-FEE    PIC XX.
           03  FILLER            PIC X(22)   VALUE
           " ) CAN NOT BE CHARGED!".

       01  NCINSR-LIST.
           03  FILLER                    PIC X(28) VALUE
           "1CLLN1 1CLDT1 1CLLN2 1CLDT2 ".
           03  FILLER                    PIC X(28) VALUE
           "1AHLN1 1AHDT1 1AHLN2 1AHDT2 ".
           03  FILLER                    PIC X(28) VALUE
           "1PPLN1 1PPDT1 1PPLN2 1PPDT2 ".
           03  FILLER                    PIC X(28) VALUE
           "2CLLN1 2CLDT1 2CLLN2 2CLDT2 ".
           03  FILLER                    PIC X(28) VALUE
           "2AHLN1 2AHDT1 2AHLN2 2AHDT2 ".
           03  FILLER                    PIC X(28) VALUE
           "2PPLN1 2PPDT1 2PPLN2 2PPDT2 ".
           03  FILLER                    PIC X(28) VALUE
           "3CLLN1 3CLDT1 3CLLN2 3CLDT2 ".
           03  FILLER                    PIC X(28) VALUE
           "3AHLN1 3AHDT1 3AHLN2 3AHDT2 ".
           03  FILLER                    PIC X(28) VALUE
           "3PPLN1 3PPDT1 3PPLN2 3PPDT2 ".
           03  FILLER                    PIC X(28) VALUE
           "4CLLN1 4CLDT1 4CLLN2 4CLDT2 ".
           03  FILLER                    PIC X(28) VALUE
           "4AHLN1 4AHDT1 4AHLN2 4AHDT2 ".
           03  FILLER                    PIC X(28) VALUE
           "4PPLN1 4PPDT1 4PPLN2 4PPDT2 ".
           03  FILLER                    PIC X(9)  VALUE
           "CL AH PP.".

       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".

       COPY "LIBWI/PFSWINDOWSW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBWI/NCINS_DEF.CPY".
       COPY "LIBWI/NCINS_WKS.CPY".

      ****************************************
      *     LINKAGE PROGRAM SECTION:
      ****************************************
       COPY "LIBGB/GETFMW.CPY".
       COPY "LIBCL/NCINSW.CPY"
                REPLACING ==VALUE "CL/NCINS"== BY ====.
 
       SCREEN SECTION.
       COPY "LIBWI/NCINS_SCN.CPY".

       PROCEDURE DIVISION
           USING FORM-PATHNAME NCINS-WORKERS EXIT-PATHNAME.

      *****************************
      *    MAIN PROGRAM
      *****************************
       A000-MAIN-PROGRAM SECTION.

           PERFORM SQL-CONNECT.

      D    STOP MESS.
           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE " " TO NCINS-ERRCD.

           IF NCINS-TYPE-CD = "R"
              PERFORM T020-NCINSR-SCAN
              GO TO END-ROUTINE.

           MOVE "SEARCHING FOR CURRENT AND RENEWED LOANS" TO MESS.
           PERFORM SEND-LEGEND.

           MOVE " " TO NCINS-BAD-FEECODE-TABLE
                       NCINSR-BUF.
           MOVE ZEROS TO NCINSR-TABLE.

           PERFORM B000-TEST-NEW-MAKERS
               VARYING NCINS-SSSNO FROM 1 BY 1
      *            UNTIL NCINS-SSSNO > 4.
                   UNTIL NCINS-SSSNO > 2.

           PERFORM T000-SET-RESTRICTIONS.

       A000-EXIT.
           GO TO END-ROUTINE.


      ********************************************************
      *    TEST MAKERS FOR INS. ORIGINATION FEE CHARGED
      ********************************************************
       B000-TEST-NEW-MAKERS.
           IF NCINS-LN-SSNO(NCINS-SSSNO) NOT = 0
              PERFORM C000-FIND-ALL-LOANS
                  VARYING SSUB FROM 1 BY 1
      *               UNTIL SSUB > 4.
                      UNTIL SSUB > 2.

      ***************************************************
      *    TEST THE NEW LOAN'S (-)SOC.SEC.NO FOR EXITING
      *    LOANS, SLOT 1 TO 2
      ***************************************************
       C000-FIND-ALL-LOANS SECTION.
           MOVE NCINS-SSSNO TO NCINSR-TTBL.

           PERFORM OPEN-LN4-FILE.

           MOVE LN-PATH-OWNBR              TO LN-OWNBR.
           MOVE NCINS-LN-SSNO(NCINS-SSSNO) TO LN-SSNO(SSUB).

           PERFORM I010-START-LN-FILE.
       C000-READ-NEXT.
           PERFORM I020-READ-LN-FILE-NEXT.
           IF IO-BAD OR (LN-OWNBR NOT = LN-PATH-OWNBR)
              GO TO C000-EXIT.

           IF ( LN-POCD-NONLOAN )
              GO TO C000-READ-NEXT.

           IF LN-SSNO(SSUB) NOT = NCINS-LN-SSNO(NCINS-SSSNO)
              GO TO C000-EXIT.

           IF ( (LN-SALEFIN = "Y") OR (LN-ORGST NOT = NCINS-ORGST) )
              GO TO C000-READ-NEXT.

           IF NCINS-PRIOR-ACCTNO = 0
              GO TO C000-READ-NEXT.

      * TEST FOR ACTIVITY IN LAST 12 MONTHS:
           MOVE LN-LOANDATE      TO NUM-DATE.
           MOVE NCINS-TRANS-DATE TO SYS-DATE.
           PERFORM TIM360.
           IF ELAPSED-DAYS > 360
              GO TO C000-READ-NEXT.

           IF NOT (LN-SRCD = "PB" OR "RN" OR "RB" OR "RO")
              PERFORM D000-TEST-RESET
                  VARYING NCINSR-IINS FROM 1 BY 1
                      UNTIL NCINSR-IINS > 3
              GO TO C000-READ-NEXT.

           PERFORM E000-TEST-FEECD
               VARYING NCINS-FFEE FROM 1 BY 1
                   UNTIL NCINS-FFEE > 3.

           GO TO C000-READ-NEXT.

       C000-EXIT.
           PERFORM CLOSE-LN4-FILE.

      *******************************************
       D000-TEST-RESET SECTION.
      *******************************************
           PERFORM D010-TEST-RESET
               VARYING NCINSR-CCNT FROM 1 BY 1
                   UNTIL NCINSR-CCNT > 2.

      *******************************************
       D010-TEST-RESET SECTION.
      *******************************************
           IF NCINSR-INS-DATE(NCINSR-TTBL NCINSR-IINS NCINSR-CCNT)
                  NOT = 0
              IF LN-LOANDATE NOT < 
                   NCINSR-INS-DATE(NCINSR-TTBL NCINSR-IINS NCINSR-CCNT)
                 MOVE ZEROS TO NCINSR-INS-CNT
                                  (NCINSR-TTBL NCINSR-IINS NCINSR-CCNT)
                 MOVE " " TO NCINSR-BUF-INS-CNT
                                  (NCINSR-TTBL NCINSR-IINS NCINSR-CCNT).

      *******************************************
      * TEST FOR FEECODE MATCH IN RENEWAL LOAN TO FEECODES
      *******************************************
       E000-TEST-FEECD SECTION.
           MOVE NCINS-FFEE TO NCINSR-IINS.
           PERFORM F000-TEST-FEECD
               VARYING FSUB FROM 1 BY 1
                   UNTIL FSUB > 10.

      *******************************************
       F000-TEST-FEECD SECTION.
      *******************************************
           IF NCINS-FEECODE(NCINS-FFEE) = LN-FEECODE(FSUB)
              IF LN-FEEAMT(FSUB) NOT = 0
                 IF LN-FEEOURS(FSUB) NOT = "M"
                    MOVE 0 TO IO-FG
                    PERFORM G000-TEST-FEECD
                        VARYING NCINSR-CCNT FROM 1 BY 1
                            UNTIL (NCINSR-CCNT > 2 OR IO-BAD).

      *******************************************
       G000-TEST-FEECD SECTION.
      *******************************************
           IF NCINSR-INS-DATE(NCINSR-TTBL NCINSR-IINS NCINSR-CCNT) = 0
              MOVE LN-ACCTNO TO NCINSR-INS-ACCTNO
                                  (NCINSR-TTBL NCINSR-IINS NCINSR-CCNT)
              MOVE LN-ACCTNO TO NCINSR-BUF-INS-ACCTNO
                                  (NCINSR-TTBL NCINSR-IINS NCINSR-CCNT)
              MOVE LN-LOANDATE TO NCINSR-INS-DATE
                                  (NCINSR-TTBL NCINSR-IINS NCINSR-CCNT)
                                  DATE-YYYYMMDD
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
              MOVE DATE-MMDDYY TO NCINSR-BUF-INS-DATE
                                  (NCINSR-TTBL NCINSR-IINS NCINSR-CCNT)
              MOVE 9 TO IO-FG
           ELSE
           IF NCINSR-CCNT = 2
              IF LN-LOANDATE > NCINSR-INS-DATE
                               (NCINSR-TTBL NCINSR-IINS 1)
                 MOVE LN-ACCTNO
                   TO NCINSR-INS-ACCTNO(NCINSR-TTBL NCINSR-IINS 1)
                 MOVE LN-ACCTNO
                   TO NCINSR-BUF-INS-ACCTNO(NCINSR-TTBL NCINSR-IINS 1)
                 MOVE LN-LOANDATE
                   TO NCINSR-INS-DATE(NCINSR-TTBL NCINSR-IINS 1)
                      DATE-YYYYMMDD
                 PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
                 MOVE DATE-MMDDYY 
                   TO NCINSR-BUF-INS-DATE(NCINSR-TTBL NCINSR-IINS 1)
              ELSE
                 IF LN-LOANDATE > NCINSR-INS-DATE
                                  (NCINSR-TTBL NCINSR-IINS 2)
                    MOVE LN-ACCTNO
                     TO NCINSR-INS-ACCTNO(NCINSR-TTBL NCINSR-IINS 2)
                        NCINSR-BUF-INS-ACCTNO(NCINSR-TTBL NCINSR-IINS 2)
                    MOVE LN-LOANDATE
                     TO NCINSR-INS-DATE(NCINSR-TTBL NCINSR-IINS 2)
                        DATE-YYYYMMDD
                    PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
                    MOVE DATE-MMDDYY
                     TO NCINSR-BUF-INS-DATE(NCINSR-TTBL NCINSR-IINS 2).

      **********************************************
      *    START LOAN FILE FOR SEARCH BY BORROWER
      **********************************************
       I010-START-LN-FILE SECTION.
           IF SSUB = 1
              MOVE LN-OWNBR   TO QLN4-WBEG-OWNBR
                                 QLN4-WEND-OWNBR
              MOVE LN-SSNO(1) TO QLN4-WBEG-SSNO1
                                 QLN4-WEND-SSNO1
              PERFORM START-LN4-FILE
           ELSE
              IF SSUB = 2
                 MOVE LN-OWNBR   TO QLN5-WBEG-OWNBR
                                    QLN5-WEND-OWNBR
                 MOVE LN-SSNO(2) TO QLN5-WBEG-SSNO2
                                    QLN5-WEND-SSNO2
                 PERFORM START-LN5-FILE.
      *       ELSE
      *          IF SSUB = 3
      *             MOVE LN-OWNBR   TO QLN6-WBEG-OWNBR
      *                                QLN6-WEND-OWNBR
      *             MOVE LN-SSNO(3) TO QLN6-WBEG-SSNO3
      *                                QLN6-WEND-SSNO3
      *             PERFORM START-LN6-FILE
      *          ELSE
      *             IF SSUB = 4
      *                MOVE LN-OWNBR   TO QLN7-WBEG-OWNBR
      *                                   QLN7-WEND-OWNBR
      *                MOVE LN-SSNO(4) TO QLN7-WBEG-SSNO4
      *                                   QLN7-WEND-SSNO4
      *                PERFORM START-LN7-FILE.

      **********************************************
      *    READ-NEXT LOAN FILE FOR SEARCH BY BORROWER
      **********************************************
       I020-READ-LN-FILE-NEXT SECTION.
           IF SSUB = 1
              PERFORM READ-LN4-FILE-NEXT
           ELSE
              IF SSUB = 2
                 PERFORM READ-LN5-FILE-NEXT.
      *       ELSE
      *          IF SSUB = 3
      *             PERFORM READ-LN6-FILE-NEXT
      *          ELSE
      *             IF SSUB = 4
      *                PERFORM READ-LN7-FILE-NEXT.

      *********************************************
       T000-SET-RESTRICTIONS SECTION.
      *********************************************

           IF NOT ( NCINSR-INS-DATE(1 1 2) = 0
                     AND NCINSR-INS-DATE(2 1 2) = 0
                       AND NCINSR-INS-DATE(3 1 2) = 0
                         AND NCINSR-INS-DATE(4 1 2) = 0 )
              MOVE NCINS-FEECODE(1) TO NCINS-BAD-FEECODE(1).

           IF NOT ( NCINSR-INS-DATE(1 2 2) = 0
                     AND NCINSR-INS-DATE(2 2 2) = 0
                       AND NCINSR-INS-DATE(3 2 2) = 0
                         AND NCINSR-INS-DATE(4 2 2) = 0 )
              MOVE NCINS-FEECODE(2) TO NCINS-BAD-FEECODE(2).

           IF NOT ( NCINSR-INS-DATE(1 3 2) = 0
                     AND NCINSR-INS-DATE(2 3 2) = 0
                       AND NCINSR-INS-DATE(3 3 2) = 0
                         AND NCINSR-INS-DATE(4 3 2) = 0 )
              MOVE NCINS-FEECODE(3) TO NCINS-BAD-FEECODE(3).

           IF NCINS-BAD-FEECODE-TABLE NOT = " "
              MOVE 0 TO IO-FG
              PERFORM T010-TEST-NC-BAD-FEES
                  VARYING NCINS-BFEE FROM 1 BY 1
                      UNTIL NCINS-BFEE > 3
              IF IO-BAD
                 MOVE "BF" TO NCINS-ERRCD.

      ****************************************
       T010-TEST-NC-BAD-FEES SECTION.
      ****************************************
           IF NCINS-BAD-FEECODE(NCINS-BFEE) = " "
              GO TO T010-TEST-NC-BAD-FEES-EXIT.

           MOVE 1 TO NCINS-PFEE.
       T010-TEST-NC-BAD-FEES-AGAIN.
           IF ( NCINS-PD-FEECODE (NCINS-PFEE) =
                NCINS-BAD-FEECODE(NCINS-BFEE) )
              MOVE NCINS-BAD-FEECODE(NCINS-BFEE) TO NC-INS-BAD-FEE
              MOVE NC-INS-BAD-FEE-MESSAGE TO MESS
              PERFORM SEND-MESS-X
              MOVE 9 TO IO-FG
              IF F6-KEY
                 PERFORM T020-NCINSR-SCAN
              ELSE
                 NEXT SENTENCE
           ELSE
              IF NCINS-PFEE < 10
                 ADD 01 TO NCINS-PFEE
                 GO TO T010-TEST-NC-BAD-FEES-AGAIN.

       T010-TEST-NC-BAD-FEES-EXIT.
           EXIT.

      ******************************************
      *    RESTRICTION SCAN
      ******************************************
       T020-NCINSR-SCAN SECTION.
           MOVE 20         TO WINDOW-LINES.
           MOVE 80         TO WINDOW-COLUMNS.
           MOVE 2          TO WINDOW-BEGIN-Y.
           MOVE 0          TO WINDOW-BEGIN-X.
           MOVE "WI/NCINS" TO WINDOW-FORM-NAM.

           MOVE "NCINS" TO VDU-FORM-NAME.

           PERFORM OPEN-WINDOW.
           DISPLAY NCINS-SCREEN.

      *       MOVE NCINS-LN-SSNO(3) TO NCINSR-BUF-SSNO(3)
      *       INSPECT NCINSR-BUF-SSNO(3) REPLACING ALL "/" BY "-".
      *    IF NCINS-LN-SSNO(4) NOT = 0
      *       MOVE NCINS-LN-SSNO(4) TO NCINSR-BUF-SSNO(4)
      *       INSPECT NCINSR-BUF-SSNO(4) REPLACING ALL "/" BY "-".

           MOVE NCINS-FEECODE(1) TO NCINSR-BUF-FEECODE(1).
           MOVE NCINS-FEECODE(2) TO NCINSR-BUF-FEECODE(2).
           MOVE NCINS-FEECODE(3) TO NCINSR-BUF-FEECODE(3).

           MOVE NCINSR-BUF TO WR-BUF.
           MOVE NCINSR-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       T020-NCINSR-SCAN-ACCEPT.
           MOVE "E  A010SEL." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS NOT = "00"
              GO TO T020-NCINSR-SCAN-ACCEPT.

       T020-NCINSR-SCAN-EXIT.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY NCINS-SCREEN.
           PERFORM CLOSE-WINDOW.

      ****************************************
       FORM-RESET SECTION.
      ****************************************

      ****************************************
       SETUP-LINK-INFO SECTION.
      ****************************************

      ****************************************
       VDU-CONVERT-FIELD SECTION.
      ****************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBWI/NCINS_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ****************************************
       MISC-ROUTINES SECTION.
      ****************************************
       SEND-MESS-X.
           PERFORM SEND-LEGEND.
           PERFORM ALARM.
           MOVE "E  A010SEL." TO VDU.
           PERFORM SCREENIO.

       SEND-LEGEND.
           MOVE MESS TO WR-BUF.
           MOVE MSEL-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       COPY "LIBGB/DATER.CPY".
       COPY "LIBWI/PFSWINDOWS.CPY".
       COPY "LIBGB/SCREEN.CPY".

      *********************************
       IO-ROUTINE SECTION.
      *********************************
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

      *********************************
       END-ROUTINE SECTION.
      *********************************
           PERFORM CLOSE-FILES.
       EXIT-PROG.
       STOP-RUN.
           EXIT PROGRAM.
