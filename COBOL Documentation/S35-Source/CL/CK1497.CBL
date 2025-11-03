       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CK1497.
       DATE-WRITTEN.  101702
      **************************************************************************
      *   DIRECTORY  :  CL
      *   DESCRIPTION:  CUSTOM WORLD PROGRAM, CALLED FROM LONPL0, VERIFY
      *                 GL ACCOUNT 1497000 NETS TO 0 BEFORE ALLOWING BRANCH
      *                 TO CLOSE DAY
      *
      *   R E M E M B E R:    CHANGE LP/LPEOD5.C   LP/RECAPJ.C
      *                              LP/GLRCAP.C   LP/GLDIST.C
      * REV:
      * JTG 021203 ADDED NEW CODE FILE TYPE 'BR' -BATCH PAYMENT REFERENCE CODES
      *            BATCH PAYMENT REVISIONS                         REGACC #0006
      * GLB 030513 WORLD PR#323 ADDED NEW "CT-FILE" FOR LIBLP/LPEOD5TR
      *            CODE TRAILER G/L INTERFACE TABLE & AMOUNT
      *            ONLY OCCURS IF CD-GC-FEE-GIFTNET = "Y"
      *  CS 031111 ADD FEE-COMMISSION (COMMISSION ON FEE CODES WRLD PR#331)
      * JTG 040330 ADDED LOGIC FOR POSTING BATCH PAYMENTS TO ACCOUNT'S
      *            WITH 'LN-OTBAL'                   CENTRIX #2253
      *            ADDED BATCH-REFCD-OT-GLNO
      * BLF 070822 BARB & CINDY.  IF I-AM-WORLD & PMT POSTED BY COLLECTION
      *            BRANCH (BR# 197) ON P&L ACCOUNT, ALTER GL UPDATE SO POSTING
      *            BRANCH GETS P&L RECOVERIES POSTED TO THEIR BRANCH,
      *            WORLD PR# 551
      * BLF 070830 UNDID WORLD PR# 551
      *  CS 081216 ADDED BR-FILE, BR-USE-PARTIC-FLAT-TABLE IS TESTED IN LPEOD5TR
      *                                              REGACC PR#232
      *  CS 150812 ADD BRANCH-COMMISSION, USED IN LPEOD5TR EXCEL PR4821
      * KEC 2016.0314 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         CHANGED CD-FEE???? TO CD-GC-????.
      *  BLM 160726 IF GP-I-AM-WORLD & CD-COMM-PERCENT = 99.99, FORCE THE
      *             PAYABLE PERCENT TO 100, FOR LIVE CHECKS, WORLD PR# 1170
      *   CS 190819 PROCESS-TRC-FILE-NEXT DID NOT MISSING READ-NEXT
      *  BAH 190918 REMOVED ACCESS-CALL ON ALL TRFILES
      *   CS 200401 PROCESS-TRL-FILE, TRX,TRB,TRK,TRS,TRJ MISSING READ-NEXT
      *  BAH 20200625 ADDED OPEN-CDB1-FILE, OPEN-CD1-FILE, OPEN-LC1-FILE,
      *               OPEN-GI1-FILE, REMOVED THEM FROM LPEOD5TR, #TST001
      *  BAH 20210422 ADDED TR-POSTING-BR AND TR-OTHBR-FG FOR GLRCAP EXTRACT
      *                                                          #1496
      *  BLM 20210810 REMOVE TP-CODE, TP-POOLID NO LONGER IN KEY
      *  BAH 20220602 ADDED TR-ACTIONCD2 FOR GLRCAP EXTRACT, #1555
      *  BAH 20220830 HARD CODED START
      *  MB  20231227 CHANGES DUE TO NEW ARRAY PROCESING IN LPEOD5TR.CPY
      *
      * JKC 2024-0124 ADDED OTIS TRACE LOGGING LOGIC
      * JKC 2024-0202 ADDED OTIS TRACE LOGGING LOGIC RIGHT BEFORE THE
      *          READ OF BRFILE SO TO GET THE CORRECT PROGRAM NAME
      *          IN THE TRACE LOG.
      *
      *  MB  20240205 ADD MOVE "N" TO ARRAYBR-LOAD-ALL-SW.
      *          THIS TELLS ARRAYBR NOT TO LOAD ALL BRANCHES UP FRONT
      *          WILL LOAD INDIVIDUAL BRANCH INFO INTO ARRAY WHEN ARRAYBR-READ
      *          IS PERFORMED FOR A BRANCH 
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "LIBLP/LPFSER.CPY".
       COPY "LIBGB/GBFSPR.CPY".
       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.
       COPY "LIBLP/LPFDER.CPY".
       COPY "LIBGB/GBFDPR.CPY".

       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)CL/CK1497.CBL S35 02/02/24-14:53:46 >".
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBLP/LPWSER.CPY".
       COPY "LIBGL/GL01GI.CPY".
       COPY "LIBGL/GL01GI_SQL.CPY".
       COPY "LIBGL/GLWSGI.CPY".
       COPY "LIBLP/LP01LC.CPY".
       COPY "LIBLP/LP01LC_SQL.CPY".
       COPY "LIBLP/LPWSLC.CPY".
       COPY "LIBLP/LP01CD.CPY".
       COPY "LIBLP/LP01CD_SQL.CPY".
       COPY "LIBLP/LPWSCD.CPY".
       COPY "LIBLP/LP01CDB.CPY".
       COPY "LIBLP/LP01CDB_SQL.CPY".
       COPY "LIBLP/LPWSCDB.CPY".
       COPY "LIBLP/LP01TRP.CPY".
       COPY "LIBLP/LP01TRP_SQL.CPY".
       COPY "LIBLP/LPWSTRP.CPY".
       COPY "LIBLP/LP01TRD.CPY".
       COPY "LIBLP/LP01TRD_SQL.CPY".
       COPY "LIBLP/LPWSTRD.CPY".
       COPY "LIBLP/LP01TRC.CPY".
       COPY "LIBLP/LP01TRC_SQL.CPY".
       COPY "LIBLP/LPWSTRC.CPY".
       COPY "LIBLP/LP01TRL.CPY".
       COPY "LIBLP/LP01TRL_SQL.CPY".
       COPY "LIBLP/LPWSTRL.CPY".
       COPY "LIBLP/LP01TRX.CPY".
       COPY "LIBLP/LP01TRX_SQL.CPY".
       COPY "LIBLP/LPWSTRX.CPY".
       COPY "LIBLP/LP01TRB.CPY".
       COPY "LIBLP/LP01TRB_SQL.CPY".
       COPY "LIBLP/LPWSTRB.CPY".
       COPY "LIBLP/LP01TRJ.CPY".
       COPY "LIBLP/LP01TRJ_SQL.CPY".
       COPY "LIBLP/LPWSTRJ.CPY".
       COPY "LIBLP/LP01TRK.CPY".
       COPY "LIBLP/LP01TRK_SQL.CPY".
       COPY "LIBLP/LPWSTRK.CPY".
       COPY "LIBLP/LP01TRS.CPY".
       COPY "LIBLP/LP01TRS_SQL.CPY".
       COPY "LIBLP/LPWSTRS.CPY".

       COPY "LIBGB/ARRAYBRW.CPY".
       COPY "LIBLP/ARRAYLCW.CPY".
       COPY "LIBGL/ARRAYGIW.CPY".

       01  PR-IFN               PIC X(2)     VALUE "PR".

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    INTERNAL TRACE FORM-PATHNAME SWAPPING WORKERS.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       01  TRACE-PATH-WORKERS.
           03  TRACE-PATH-SW               PIC 9(1)      VALUE ZEROES.
           03  TRACE-LINK-FORM-PATHNAME    PIC X(22)     VALUE SPACES.
           03  TRACE-TRUE-FORM-PATHNAME    PIC X(22)     VALUE SPACES.
           03  TRACE-TRUE-FORM-PATH  REDEFINES TRACE-TRUE-FORM-PATHNAME.
               05  FILLER                  PIC X(15).
               05  TRACE-TRUE-FORM-PROGX   PIC X(6).
               05  FILLER                  PIC X(1).

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       01  FUNK-SUB             PIC 999.
      *PLSUB MUST BE PIC 9:
       01  PLSUBX.
           03  PLSUB            PIC 9.

       01  TR-DATE              PIC 9(8).
       01  TR-CODE              PIC X.
       01  TR-ACCT              PIC 9(8).
       01  TR-TYPE-TRCD         PIC XX.
       01  TR-REASON            PIC X(30).
       01  TR-CLASS             PIC 9(3).
       01  TR-REFNO             PIC X(5).
       01  TR-PLTYPE            PIC X(1).
       01  TR-POSTING-BR        PIC 9(4).
       01  TR-OTHBR-FG          PIC X(1).
       01  TR-ACTIONCD2         PIC X(4).

       01  TR-SUBKEY            PIC X(17).
       01  TRP-SUBKEY REDEFINES TR-SUBKEY.
           03  TRP-SUBKEY-ACCTNO       PIC 9(8).
           03  TRP-SUBKEY-LINE         PIC 9(3).
       01  OTHER-SUBKEY REDEFINES TR-SUBKEY.
           03  OTHER-SUBKEY-ACCTNO       PIC 9(8).
           03  OTHER-SUBKEY-LAWCODE      PIC 9(2).
           03  OTHER-SUBKEY-LINE         PIC 9(3).

       01  BATCH-REFCD          PIC X(5)      VALUE LOW-VALUES.
       01  BATCH-REFCD-GLNO     PIC 9(11)     VALUE 0.
       01  BATCH-REFCD-OT-GLNO  PIC 9(11)     VALUE 0.

      **********************
      *   LINE WORKERS     *
      **********************
       01  LN-WORKERS.
           03  LN-FEED          PIC 999      VALUE 0.
           03  LN-COUNT         PIC 999      VALUE 99.
           03  LN-WK            PIC 999      VALUE 0.
       01  PAGE-NUMBER          PIC 9(4)     VALUE 0.

       01  ER-FEED              PIC 9        VALUE 1.

       01  ERRCD                PIC X.


      **********************
       01  SUB                  PIC 9(6) COMP.
       01  L1                   PIC 9(6) COMP.
       01  FIRST-TIME           PIC X        VALUE "Y".
       01  CLEARING-WK          PIC S9(10)V99  COMP.
       01  FEE-COMMISSION       PIC S9(4)V99.
       01  HOLD-BRNO            PIC 9(4).

       01  DF-TEST              PIC X        VALUE " ".
           88  DF-POSTING                    VALUE "Y".

      ****************************************************
      * TOTALS TABLE:  LEVEL 1 = 1 DAY FOR 1 GL ACCOUNT
      *                LEVEL 2 = GL ACCOUNT TOTALS
      *                LEVEL 3 = BRANCH
      *                LEVEL 4 = GRAND
      ****************************************************
       01  GLNO                 PIC 9(11)     VALUE 0.
       01  GLNOX               REDEFINES GLNO.
           03  FILLER           PIC X(4).
           03  FILLER           PIC X(3).
           03  GLOTHBR          PIC 9999.
       01  GLNOZ               REDEFINES GLNO.
           03  GLNO-BR          PIC 9999.
           03  GLNO-NUMBER      PIC 9(7).
       01  FILLER               REDEFINES GLNO.
           03  GLNO-BRANCH      PIC 9(4).
           03  GLNO-ACCT        PIC 9(7).

       01  GLAMT                PIC S9(9)V99 VALUE 0.
       01  GLWK                 PIC S9(9)V99 VALUE 0.
       01  HOLD-PL-EXP          PIC S9(9)V99 VALUE 0.
       01  ERROR-FG             PIC X VALUE "N".

       01  HOLD-COMM-PERCENT    PIC 999V99 VALUE 0.

      ***********************************
      *     P R I N T   B U F F E R S   *
      ***********************************
       01  HEAD1.
           03  H-DATE           PIC X(8)     VALUE " ".
           03  FILLER           PIC XX       VALUE " ".
           03  H-HR             PIC 99       VALUE 0.
           03  FILLER           PIC X        VALUE ":".
           03  H-MIN            PIC 99       VALUE 0.
           03  FILLER           PIC X(31)    VALUE " ".
           03  H-NAME           PIC X(40)    VALUE " ".
           03  FILLER           PIC X(37)    VALUE " ".
           03  FILLER           PIC X(5)     VALUE "PAGE ".
           03  H-PAGE-NO        PIC ZZZ9.

       01  HEAD2.
           03  FILLER           PIC X(10)    VALUE "USER ID:".
           03  H-USERID         PIC X(12).
           03  FILLER           PIC X(24)    VALUE " ".
           03  H-TITLE          PIC X(41)    VALUE
               " LOAN PROCESSING GL DISTRIBUTION JOURNAL ".

       01  HEAD3.
           03  FILLER           PIC X(10) VALUE "BRANCH :".
           03  H-BRNO           PIC ZZ99.
           03  FILLER           PIC X VALUE " ".
           03  H-BRNAME         PIC X(20).
           03  FILLER           PIC X(27) VALUE " ".

       01  HEAD4.
           03  FILLER           PIC X(50)   VALUE
           "G/L ACCOUNT   DESCRIPTION                         ".
           03  FILLER           PIC X(12)   VALUE "TRANS DATE".
           03  FILLER           PIC X(33)   VALUE
               "SOURCE/TRANSACTION INFORMATION ".
           03  FILLER           PIC X(17)   VALUE "ACCT/CK #".
           03  FILLER           PIC X(14)   VALUE "DEBIT         ".
           03  FILLER           PIC X(6)    VALUE "CREDIT".

       01  DETAIL-LINE          PIC X(132) VALUE " ".


       COPY "LIBLP/LPEDPATH.CPY".
       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/GPENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      ****************************************
      *     P R O G R A M   C O N T R O L    *
      ****************************************
       COPY "LIBGB/GETFMW.CPY".
       COPY "LIBCL/CK1497W.CPY".
       PROCEDURE DIVISION
            USING FORM-PATHNAME CK1497-WORKERS EXIT-PATHNAME.
       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
               PR-FILE ER-FILE.
       COPY "LIBGB/DECLARE.CPY".
           PERFORM CLOSE-TRX1-FILE.
           PERFORM CLOSE-TRS1-FILE.
           PERFORM CLOSE-TRP1-FILE.
           PERFORM CLOSE-TRL1-FILE.
           PERFORM CLOSE-TRK1-FILE.
           PERFORM CLOSE-TRJ1-FILE.
           PERFORM CLOSE-TRD1-FILE.
           PERFORM CLOSE-TRC1-FILE.
           PERFORM CLOSE-TRB1-FILE.
           PERFORM CLOSE-LC1-FILE.
           PERFORM CLOSE-CDB1-FILE.
           PERFORM CLOSE-CD1-FILE.
           PERFORM CLOSE-GI1-FILE.
           PERFORM CLOSE-BR-FILE.
           PERFORM CLOSE-PR-FILE.
           CLOSE ER-FILE.
       COPY "LIBGB/DECLAR2.CPY".

      ************************************
       MAIN-MODULE SECTION.
      ************************************

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    SOME COPY MEMBERS, LIKE ARRAYBR, USE FORM-PATHNAME WHICH
      *    IT MAY NOT BE ACCURATE.  SO NEED TO SAVE THE FORM-PATHNAME
      *    (TRACE-LINK-FORM-PATHNAME).  THEN POPULATE
      *    TRACE-TRUE-FORM-PATHNAME USING FORM-PATHNAME THEN SET
      *    TRACE-TRUE-FORM-PROGX WITH THE TRUE PROGRAM NAME.
      *    BEFORE EXECUTING A SECTION/PARAGRAPH IN A COPY MEMBER THAT
      *    USES A TRACE FUNCTION USING FORM-PATHNAME, MUST MOVE
      *    'TRUE' PATH INTO FORM-PATHNAME THEN RESTORE IT AFTERWARDS
      *    USING 'LINK' PATH.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE FORM-PATHNAME    TO TRACE-LINK-FORM-PATHNAME
                                    TRACE-TRUE-FORM-PATHNAME.
           MOVE "CK1497"         TO TRACE-TRUE-FORM-PROGX.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    NEED TO SET EXT-TCLP-TRACE-FORM-PATH BEFORE GOING INTO
      *    'SQL-CONNECT' SO THAT IT CAN GET THE EXACT PROGRAM NAME
      *    IN THE TRACE LOG.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE TRACE-TRUE-FORM-PATHNAME TO EXT-TCLP-TRACE-FORM-PATH.
           PERFORM SQL-CONNECT.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE FORM-PATHNAME TO FORM-PATH.

           PERFORM INITIALIZATION.

           MOVE TRACE-TRUE-FORM-PATHNAME TO EXT-TCLP-TRACE-FORM-PATH.
           MOVE CK1497-BRNO   TO BR-NO HOLD-BRNO.
           PERFORM READ-BR-FILE.
           IF IO-FG = 9
              GO TO IO-ERROR.

           MOVE EXT-FILPATH-BASE           TO FILPATH-BASE.
           MOVE BR-MACHINE                 TO FILPATH-MACHINE.
           MOVE BR-DATA-PATH               TO FILPATH-DATA.

           MOVE "B"                        TO TRP-PATH-OVERRIDE
                                              TRD-PATH-OVERRIDE
                                              TRC-PATH-OVERRIDE
                                              TRL-PATH-OVERRIDE
                                              TRX-PATH-OVERRIDE
                                              TRB-PATH-OVERRIDE
                                              TRJ-PATH-OVERRIDE
                                              TRK-PATH-OVERRIDE
                                              TRS-PATH-OVERRIDE.

      * LOAD GI TABLE WITH BRANCH 1497 DATA
           MOVE TRACE-TRUE-FORM-PATHNAME TO FORM-PATHNAME.
           MOVE BR-NO                    TO ARRAYGI-GI-BRANCH.
           PERFORM ARRAYGI-LOAD.
           MOVE TRACE-LINK-FORM-PATHNAME TO FORM-PATHNAME.

      * TRANSACTIONS
           PERFORM PROCESS-TRP-FILE.
      * DEPOSITS
           PERFORM PROCESS-TRD-FILE.
      * CHECKS
           PERFORM PROCESS-TRC-FILE.
      * DIRECT LOANS
           PERFORM PROCESS-TRL-FILE.
      * TRANSFERS
           PERFORM PROCESS-TRX-FILE.
      * BRANCH (BULK) TRANSFERS
           PERFORM PROCESS-TRB-FILE.
      * BRANCH (CLASS) TRANSFERS
           PERFORM PROCESS-TRJ-FILE.
      * ACCOUNT CONVERSIONS
           PERFORM PROCESS-TRK-FILE.
      * SALES FINANCE
           PERFORM PROCESS-TRS-FILE.

           MOVE 99 TO LN-COUNT.

           GO TO END-PROGRAM.

      *****************************************
       PROCESS-TRP-FILE SECTION.
      *****************************************
           PERFORM OPEN-TRP1-FILE.

           MOVE CK1497-BRNO         TO TP-BRNO
                                       QTRP1-WBEG-BRNO
                                       QTRP1-WEND-BRNO.
           MOVE CK1497-TRANS-DATE   TO TP-DATE SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QTRP1-WBEG-DATE
                                       QTRP1-WEND-DATE.

           MOVE 0                   TO TP-CLASS TP-ACCTNO TP-LINE
                                       QTRP1-WBEG-CLASS
                                       QTRP1-WBEG-ACCTNO
                                       QTRP1-WBEG-LINE.
           MOVE ALL "9"             TO QTRP1-WEND-CLASS
                                       QTRP1-WEND-ACCTNO
                                       QTRP1-WEND-LINE.

           MOVE TP-DATE              TO NDTE-DATE
           MOVE 1                    TO NDTE-HOLD
           PERFORM INCREMENT-DAYS
           MOVE NDTE-DATE            TO SQL-DATE-YYYYMMDD
           PERFORM SQL-SET-DATE
           MOVE SQL-DATE-YYYY-MM-DD  TO QTRP1-WSBEG-DATE.

           IF ( TP-CLASS = 999 )
              MOVE TP-CLASS          TO QTRP1-WSBEG-CLASS
           ELSE
              COMPUTE QTRP1-WSBEG-CLASS  = TP-CLASS + 1.

           IF ( TP-ACCTNO = 99999999 )
              MOVE TP-ACCTNO         TO QTRP1-WSBEG-ACCTNO
           ELSE
              COMPUTE QTRP1-WSBEG-ACCTNO = TP-ACCTNO + 1.

           IF ( TP-LINE = 999 )
              MOVE TP-LINE           TO QTRP1-WSBEG-LINE
           ELSE
              COMPUTE QTRP1-WSBEG-LINE   = TP-LINE + 1.

           PERFORM START-TRP1-FILE.

       PROCESS-TRP-NEXT.
           PERFORM READ-TRP1-FILE-NEXT.
           IF (IO-FG NOT = 0) OR (TRP-PATH-OWNBR NOT = TP-BRNO)
              GO TO EXIT-PROCESS-TRP-FILE.

           IF TP-DATE NOT = CK1497-TRANS-DATE
              GO TO EXIT-PROCESS-TRP-FILE.

           PERFORM TRP1-DETAIL.

           GO TO PROCESS-TRP-NEXT.

       EXIT-PROCESS-TRP-FILE.
           PERFORM CLOSE-TRP1-FILE.

      *****************************************
       PROCESS-TRD-FILE SECTION.
      *****************************************
           PERFORM OPEN-TRD1-FILE.

           MOVE CK1497-BRNO         TO TD-BRNO
                                       QTRD1-WBEG-BRNO
                                       QTRD1-WEND-BRNO.
           MOVE CK1497-TRANS-DATE   TO TD-DATE
                                       SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QTRD1-WBEG-DATE
                                       QTRD1-WEND-DATE.

           PERFORM START-TRD1-FILE.

       PROCESS-TRD-NEXT.
           PERFORM READ-TRD1-FILE-NEXT.
           IF (IO-FG NOT = 0) OR (TRD-PATH-OWNBR NOT = TD-BRNO)
              GO TO EXIT-PROCESS-TRD-FILE.

           IF TD-DATE NOT = CK1497-TRANS-DATE
              GO TO EXIT-PROCESS-TRD-FILE.

           PERFORM TRD1-DETAIL.

           GO TO PROCESS-TRD-NEXT.

       EXIT-PROCESS-TRD-FILE.
           PERFORM CLOSE-TRD1-FILE.

      ******************************************
       PROCESS-TRC-FILE SECTION.
      ******************************************
           PERFORM OPEN-TRC1-FILE.

           MOVE CK1497-BRNO         TO TC-BRNO
                                       QTRC1-WBEG-BRNO
                                       QTRC1-WEND-BRNO.
           MOVE CK1497-TRANS-DATE   TO TC-KEY-DATE
                                       SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QTRC1-WBEG-KEY-DATE
                                       QTRC1-WEND-KEY-DATE.

           MOVE 0                   TO TC-CKNO TC-CLASS TC-SEQ
                                       QTRC1-WBEG-CKNO
                                       QTRC1-WBEG-CLASS
                                       QTRC1-WBEG-SEQ.

           MOVE ALL "9"             TO QTRC1-WEND-CKNO
                                       QTRC1-WEND-CLASS
                                       QTRC1-WEND-SEQ.

           PERFORM START-TRC1-FILE.
       PROCESS-TRC-NEXT.
           PERFORM READ-TRC1-FILE-NEXT.
           IF (IO-FG NOT = 0) OR (TRC-PATH-OWNBR NOT = TC-BRNO)
              GO TO EXIT-PROCESS-TRC-FILE.

           IF TC-KEY-DATE NOT = CK1497-TRANS-DATE
              GO TO EXIT-PROCESS-TRC-FILE.

           PERFORM TRC1-DETAIL.

           GO TO PROCESS-TRC-NEXT.

       EXIT-PROCESS-TRC-FILE.
           PERFORM CLOSE-TRC1-FILE.
           

      ******************************************
       PROCESS-TRL-FILE SECTION.
      ******************************************
           PERFORM OPEN-TRL1-FILE.

           MOVE CK1497-BRNO         TO TL-BRNO
                                       QTRL1-WBEG-BRNO
                                       QTRL1-WEND-BRNO.
           MOVE CK1497-TRANS-DATE   TO TL-DATE SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QTRL1-WBEG-DATE
                                       QTRL1-WEND-DATE.
 
           MOVE 0                   TO TL-CLASS TL-ACCTNO
                                       TL-LAWCODE TL-LINE
                                       QTRL1-WBEG-CLASS
                                       QTRL1-WBEG-ACCTNO
                                       QTRL1-WBEG-LAWCODE
                                       QTRL1-WBEG-LINE.

           MOVE ALL "9"             TO QTRL1-WEND-CLASS
                                       QTRL1-WEND-ACCTNO
                                       QTRL1-WEND-LAWCODE
                                       QTRL1-WEND-LINE.
           PERFORM START-TRL1-FILE.
       PROCESS-TRL-NEXT.
           PERFORM READ-TRL1-FILE-NEXT.
           IF (IO-FG NOT = 0) OR (TRL-PATH-OWNBR NOT = TL-BRNO)
              GO TO EXIT-PROCESS-TRL-FILE.

           IF TL-DATE NOT = CK1497-TRANS-DATE
              GO TO EXIT-PROCESS-TRL-FILE.

           PERFORM TRL1-DETAIL.

           GO TO PROCESS-TRL-NEXT.

       EXIT-PROCESS-TRL-FILE.
           PERFORM CLOSE-TRL1-FILE.
           
      ******************************************
       PROCESS-TRX-FILE SECTION.
      ******************************************
           PERFORM OPEN-TRX1-FILE.

           MOVE CK1497-BRNO         TO TX-BRNO
                                       QTRX1-WBEG-BRNO
                                       QTRX1-WEND-BRNO.
           MOVE CK1497-TRANS-DATE   TO TX-DATE SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QTRX1-WBEG-DATE
                                       QTRX1-WEND-DATE.
 
           MOVE 0                   TO TX-CLASS TX-ACCTNO
                                       TX-LAWCODE TX-LINE
                                       QTRX1-WBEG-CLASS
                                       QTRX1-WBEG-ACCTNO
                                       QTRX1-WBEG-LAWCODE
                                       QTRX1-WBEG-LINE.

           MOVE ALL "9"             TO QTRX1-WEND-CLASS
                                       QTRX1-WEND-ACCTNO
                                       QTRX1-WEND-LAWCODE
                                       QTRX1-WEND-LINE.

           PERFORM START-TRX1-FILE.
       PROCESS-TRX-NEXT.
           PERFORM READ-TRX1-FILE.
           IF (IO-FG NOT = 0) OR (TRX-PATH-OWNBR NOT = TX-BRNO)
              GO TO EXIT-PROCESS-TRX-FILE.

           IF TX-DATE NOT = CK1497-TRANS-DATE
              GO TO EXIT-PROCESS-TRX-FILE.

           PERFORM TRX1-DETAIL.

           GO TO PROCESS-TRX-NEXT.

       EXIT-PROCESS-TRX-FILE.
           PERFORM CLOSE-TRX1-FILE.

      ******************************************
       PROCESS-TRB-FILE SECTION.
      ******************************************
           PERFORM OPEN-TRB1-FILE.

           MOVE CK1497-BRNO         TO TB-BRNO
                                       QTRB1-WBEG-BRNO
                                       QTRB1-WEND-BRNO.
           MOVE CK1497-TRANS-DATE   TO TB-DATE SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QTRB1-WBEG-DATE
                                       QTRB1-WEND-DATE.
 
           MOVE 0                   TO TB-CLASS TB-ACCTNO
                                       TB-LAWCODE TB-LINE
                                       QTRB1-WBEG-CLASS
                                       QTRB1-WBEG-ACCTNO
                                       QTRB1-WBEG-LAWCODE
                                       QTRB1-WBEG-LINE.

           MOVE ALL "9"             TO QTRB1-WEND-CLASS
                                       QTRB1-WEND-ACCTNO
                                       QTRB1-WEND-LAWCODE
                                       QTRB1-WEND-LINE.
           PERFORM START-TRB1-FILE.
       PROCESS-TRB-NEXT.
           PERFORM READ-TRB1-FILE-NEXT.
           IF (IO-FG NOT = 0) OR (TRB-PATH-OWNBR NOT = TB-BRNO)
              GO TO EXIT-PROCESS-TRB-FILE.

           IF TB-DATE NOT = CK1497-TRANS-DATE
              GO TO EXIT-PROCESS-TRB-FILE.

           PERFORM TRB1-DETAIL.

           GO TO PROCESS-TRB-NEXT.

       EXIT-PROCESS-TRB-FILE.
           PERFORM CLOSE-TRB1-FILE.

      ******************************************
       PROCESS-TRK-FILE SECTION.
      ******************************************
           PERFORM OPEN-TRK1-FILE.

           MOVE CK1497-BRNO         TO TK-BRNO
                                       QTRK1-WBEG-BRNO
                                       QTRK1-WEND-BRNO.
           MOVE CK1497-TRANS-DATE   TO TK-DATE SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QTRK1-WBEG-DATE
                                       QTRK1-WEND-DATE.
 
           MOVE 0                   TO TK-CLASS TK-ACCTNO
                                       TK-LAWCODE TK-LINE
                                       QTRK1-WBEG-CLASS
                                       QTRK1-WBEG-ACCTNO
                                       QTRK1-WBEG-LAWCODE
                                       QTRK1-WBEG-LINE.

           MOVE ALL "9"             TO QTRK1-WEND-CLASS
                                       QTRK1-WEND-ACCTNO
                                       QTRK1-WEND-LAWCODE
                                       QTRK1-WEND-LINE.
           PERFORM START-TRK1-FILE.
       PROCESS-TRK-NEXT.
           PERFORM READ-TRK1-FILE-NEXT.
           IF (IO-FG NOT = 0) OR (TRK-PATH-OWNBR NOT = TK-BRNO)
              GO TO EXIT-PROCESS-TRK-FILE.

           IF TK-DATE NOT = CK1497-TRANS-DATE
              GO TO EXIT-PROCESS-TRK-FILE.

           PERFORM TRK1-DETAIL.

           GO TO PROCESS-TRK-NEXT.

       EXIT-PROCESS-TRK-FILE.
           PERFORM CLOSE-TRK1-FILE.

      ******************************************
       PROCESS-TRJ-FILE SECTION.
      ******************************************
           PERFORM OPEN-TRJ1-FILE.

           MOVE CK1497-BRNO         TO TJ-BRNO
                                       QTRJ1-WBEG-BRNO
                                       QTRJ1-WEND-BRNO.
           MOVE CK1497-TRANS-DATE   TO TJ-DATE SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QTRJ1-WBEG-DATE
                                       QTRJ1-WEND-DATE.
 
           MOVE 0                   TO TJ-CLASS TJ-ACCTNO
                                       TJ-LAWCODE TJ-LINE
                                       QTRJ1-WBEG-CLASS
                                       QTRJ1-WBEG-ACCTNO
                                       QTRJ1-WBEG-LAWCODE
                                       QTRJ1-WBEG-LINE.

           MOVE ALL "9"             TO QTRJ1-WEND-CLASS
                                       QTRJ1-WEND-ACCTNO
                                       QTRJ1-WEND-LAWCODE
                                       QTRJ1-WEND-LINE.
           PERFORM START-TRJ1-FILE.
       PROCESS-TRJ-NEXT.
           PERFORM READ-TRJ1-FILE-NEXT.
           IF (IO-FG NOT = 0) OR (TRJ-PATH-OWNBR NOT = TJ-BRNO)
              GO TO EXIT-PROCESS-TRJ-FILE.

           IF TJ-DATE NOT = CK1497-TRANS-DATE
              GO TO EXIT-PROCESS-TRJ-FILE.

           PERFORM TRJ1-DETAIL.

           GO TO PROCESS-TRJ-NEXT.

       EXIT-PROCESS-TRJ-FILE.
           PERFORM CLOSE-TRJ1-FILE.

      ******************************************
       PROCESS-TRS-FILE SECTION.
      ******************************************
           PERFORM OPEN-TRS1-FILE.

           MOVE CK1497-BRNO         TO TS-BRNO
                                       QTRS1-WBEG-BRNO
                                       QTRS1-WEND-BRNO.
           MOVE CK1497-TRANS-DATE   TO TS-DATE SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QTRS1-WBEG-DATE
                                       QTRS1-WEND-DATE.
 
           MOVE 0                   TO TS-CLASS TS-ACCTNO
                                       TS-LAWCODE TS-LINE
                                       QTRS1-WBEG-CLASS
                                       QTRS1-WBEG-ACCTNO
                                       QTRS1-WBEG-LAWCODE
                                       QTRS1-WBEG-LINE.

           MOVE ALL "9"             TO QTRS1-WEND-CLASS
                                       QTRS1-WEND-ACCTNO
                                       QTRS1-WEND-LAWCODE
                                       QTRS1-WEND-LINE.
           PERFORM START-TRS1-FILE.
       PROCESS-TRS-NEXT.
           PERFORM READ-TRS1-FILE-NEXT.
           IF (IO-FG NOT = 0) OR (TRS-PATH-OWNBR NOT = TS-BRNO)
              GO TO EXIT-PROCESS-TRS-FILE.

           IF TS-DATE NOT = CK1497-TRANS-DATE
              GO TO EXIT-PROCESS-TRS-FILE.

           PERFORM TRS1-DETAIL.

           GO TO PROCESS-TRS-NEXT.

       EXIT-PROCESS-TRS-FILE.
           PERFORM CLOSE-TRS1-FILE.

      ************************************
       INITIALIZATION SECTION.
      ************************************

           PERFORM GET-GPENV.

           MOVE 0 TO CK1497-AMOUNT.

           MOVE EXT-CONAME-CONAME  TO H-NAME.
           MOVE EXT-CONAME-SYSDATE TO H-DATE.
           MOVE EXT-CONAME-USER-ID TO H-USERID.

           PERFORM OPEN-BR-FILE.
           PERFORM OPEN-CDB1-FILE.
           PERFORM OPEN-CD1-FILE.
           PERFORM OPEN-LC1-FILE.
           PERFORM OPEN-GI1-FILE.

      * THESE FOR LPEOD5TR.CPY  - CHANGE MB 2023-1227
           MOVE TRACE-TRUE-FORM-PATHNAME TO FORM-PATHNAME.
           MOVE "N" TO ARRAYBR-LOAD-ALL-SW.
           PERFORM ARRAYBR-LOAD.
           PERFORM ARRAYLC-LOAD.
           MOVE TRACE-LINK-FORM-PATHNAME TO FORM-PATHNAME.

           GO TO EXIT-INITIALIZE.
       ENDIT.
           MOVE "X" TO ERRCD.
       EXIT-INITIALIZE.
           EXIT.

      ************************************
       UPDATE-WORK-FILE SECTION.
      ************************************

      * CREATE "G" ED1

           IF GLAMT = 0
              GO TO EXIT-UPDATE-WORK-FILE.

           IF GLNO-BRANCH < CK1497-BRNO
              OR GLNO-BRANCH > CK1497-BRNO
                 GO TO EXIT-UPDATE-WORK-FILE.

           IF GLNO-ACCT NOT = 1497000
              GO TO EXIT-UPDATE-WORK-FILE.

           ADD GLAMT TO CK1497-AMOUNT.

       EXIT-UPDATE-WORK-FILE.
           MOVE 0 TO GLAMT GLNO.

      ************************************
       WRITE-DETAIL-LINE SECTION.
      ************************************

           ADD LN-FEED LN-COUNT GIVING LN-WK.
           IF LN-WK GREATER THAN EXT-CONAME-RPT-LINES
              PERFORM HEAD-PAGE.
           MOVE DETAIL-LINE TO PR-REC.
           PERFORM WRITE-PR-REC.
           ADD LN-FEED TO LN-COUNT.
           MOVE 1 TO LN-FEED.
           MOVE " " TO DETAIL-LINE.

      ************************************
       HEAD-PAGE SECTION.
      ************************************

           MOVE "N" TO FIRST-TIME.
           ADD 1 TO PAGE-NUMBER.
           MOVE PAGE-NUMBER TO H-PAGE-NO.
           PERFORM GET-TIME.
           MOVE T-HH TO H-HR.
           MOVE T-MM TO H-MIN.

           MOVE HEAD1 TO PR-REC.
           MOVE 99 TO LN-FEED.
           PERFORM WRITE-PR-REC.

           MOVE HEAD2 TO PR-REC.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-PR-REC.

           MOVE BR-NO   TO H-BRNO.
           MOVE BR-NAME TO H-BRNAME.
           MOVE HEAD3 TO PR-REC.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-PR-REC.

           MOVE HEAD4 TO PR-REC.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-PR-REC.

       END-HEAD-PAGE.
           MOVE 5 TO LN-COUNT.
           MOVE 2 TO LN-FEED.

      ******************************************************************
      *
      *    C O P Y - L I B R A R Y - S E C T I O N S
      *
      ******************************************************************
       COPY "LIBGB/TIMEIO.CPY".
       COPY "LIBGB/GPENV.CPY".
       COPY "LIBLP/LPEOD5TR.CPY".
       COPY "LIBGB/DATER.CPY".

      ************************************
       MISC-PARA SECTION.
      ************************************
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/SYSTEM.CPY".
       COPY "LIBGB/GETPR.CPY".
      *=================================================================
      *=================================================================
      *=================================================================
       MISC-TRACE-FORM-PATHNAME.

      *-----------------------------------------------------------------
      * CHECK-UPDATE-BR & TD-UPD-BR ARE DUMMY PARAGRAPHS
      * BECAUSE COPY MEMBER LPEOD5TR PERFORMS THEM WHEN UPDATING.
      *-----------------------------------------------------------------
       CHECK-UPDATE-BR.
       TD-UPD-BR.
       SAVE-ERROR-MSG.
       GET-CODE.
           MOVE "GC"   TO CDB-TYPE.
      *--> SET BEFORE     CDB-BRACH.
      *--> SET BEFORE     CDB-CODE.
           PERFORM READ-CDB1-FILE.
       TEST-BR-FEE-GL-BRNO.
           IF BR-FEE-GL-BRNO NUMERIC
              IF BR-FEE-GL-BRNO NOT = 0
                 MOVE BR-FEE-GL-BRNO TO GLNO-BR.
       SETUP-LINK-INFO.

       CREATE-LOG.
      
       CONVERT-TIME.
           IF H-AND-M NOT GREATER THAN 1159
              MOVE "AM" TO T-AMPM
           ELSE
              MOVE "PM" TO T-AMPM.
           IF H-AND-M IS NOT LESS THAN 1300
              SUBTRACT 1200 FROM H-AND-M.
           IF CUR-HH = 0
              MOVE 12 TO CUR-HH.
           MOVE CUR-HH TO T-HH.
           MOVE CUR-MM TO T-MM.
           MOVE ":" TO T-COL.
           MOVE " " TO T-SPACE.

       COPY "LIBGB/ARRAYBR.CPY".
       COPY "LIBLP/ARRAYLC.CPY".
       COPY "LIBGL/ARRAYGI.CPY".

      *******************************************
       IO-ROUTINES SECTION.
      *******************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBLP/LPTRXGS_SQL.CPY".
       COPY "LIBLP/LPTRSGS_SQL.CPY".
       COPY "LIBLP/LPTRPGS_SQL.CPY".
       COPY "LIBLP/LPTRLGS_SQL.CPY".
       COPY "LIBLP/LPTRKGS_SQL.CPY".
       COPY "LIBLP/LPTRJGS_SQL.CPY".
       COPY "LIBLP/LPTRDGS_SQL.CPY".
       COPY "LIBLP/LPTRCGS_SQL.CPY".
       COPY "LIBLP/LPTRBGS_SQL.CPY".
       COPY "LIBLP/LPLCGS_SQL.CPY".
       COPY "LIBLP/LPCDBGS_SQL.CPY".
       COPY "LIBLP/LPCDGS_SQL.CPY".
       COPY "LIBGL/GLGIGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
      *******************************************
       COPY "LIBGB/GBPRINT.CPY".
       COPY "LIBLP/LPPRINT.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBLP/LPTRP1RN.CPY".
       COPY "LIBLP/LPTRD1RN.CPY".
       COPY "LIBLP/LPTRC1RN.CPY".
       COPY "LIBLP/LPTRL1RN.CPY".
       COPY "LIBLP/LPTRX1RN.CPY".
       COPY "LIBLP/LPTRB1RN.CPY".
       COPY "LIBLP/LPTRJ1RN.CPY".
       COPY "LIBLP/LPTRK1RN.CPY".
       COPY "LIBLP/LPTRS1RN.CPY".
       COPY "LIBGL/GLGI1RN.CPY".
       COPY "LIBLP/LPLC1RN.CPY".
       COPY "LIBLP/LPCD1RN.CPY".
       COPY "LIBLP/LPCDB1RN.CPY".
       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO ERRCD.


      *******************************************
       END-PROGRAM SECTION.
      *******************************************
       END-RUN.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    NEED TO SET EXT-TCLP-TRACE-FORM-PATH BEFORE GOING INTO
      *    'CLOSE-FILES' (LIBGB/*DECLARE*) SO THAT IT CAN GET THE EXACT
      *    PROGRAM NAME IN THE TRACE LOG; NEED TO MOVE SPACES INTO
      *    EXT-TCLP-TRACE-FORM-PATH AFTER 'CLOSE-FILES' TO RESUME
      *    USING FORM-PATHNAME.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE FORM-PATHNAME TO EXT-TCLP-TRACE-FORM-PATH.
           MOVE "CK1497"      TO EXT-TCLP-TRACE-FORM-PROGX.
           PERFORM CLOSE-FILES.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       EXIT-PROG.
           EXIT.

       STOP-RUN.
           EXIT PROGRAM.
