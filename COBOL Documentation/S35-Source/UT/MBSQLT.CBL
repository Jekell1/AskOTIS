       IDENTIFICATION DIVISION.
       PROGRAM-ID.   MBSQLT.
       AUTHOR.       MB.
       DATE-WRITTEN. 11-10-2023.
      ******************************************************************
      *
      * IDENTIFICATION: UT/MBSQLT.CBL
      *
      * DESCRIPTION   : TEST PROGRAM
      *
      *=================================================================
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "LIBGB/GBFSPR.CPY".
       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.
       COPY "LIBGB/GBFDPR.CPY".

       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(37) VALUE
           "@(#)UT/MBSQLT S35 04/02/25-09:42:58 >".

       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBLP/LP01LN.CPY".
       COPY "LIBLP/LP01LN_SQL.CPY".
       COPY "LIBLP/LP01NO.CPY".
       COPY "LIBLP/LP01NO_SQL.CPY".
       COPY "LIBLP/LP01BW.CPY".
       COPY "LIBLP/LP01BW_SQL.CPY".
       COPY "LIBLP/LP01LC.CPY".
       COPY "LIBLP/LP01LC_SQL.CPY".
       COPY "LIBLP/LP01CD.CPY".
       COPY "LIBLP/LP01CD_SQL.CPY".
       COPY "LIBLP/LP01LP.CPY".
       COPY "LIBLP/LP01LP_SQL.CPY".
       COPY "LIBLP/LP01SP.CPY".
       COPY "LIBLP/LP01SPA.CPY".
       COPY "LIBLP/LP01SPB.CPY".
       COPY "LIBLP/LP01SPC.CPY".
       COPY "LIBLP/LP01SP_SQL.CPY".
       COPY "LIBLP/ARRAYSPW.CPY". 
       COPY "LIBLP/LPWSSP.CPY".
       COPY "LIBGB/EOBUF_EXT.CPY".
       COPY "LIBLP/LPCEPPW.CPY".
       COPY "LIBGB/SPPDTHW.CPY".
       COPY "LIBLP/LPPDUEW.CPY".
       COPY "LIBLP/LPMDTEW.CPY".
       COPY "LIBGB/GB01GP.CPY".
       COPY "LIBLP/LP01LTP.CPY".
       COPY "LIBLP/LPWSLTP.CPY".
       COPY "LIBLP/LP01LTP_SQL.CPY".
       COPY "LIBLP/LPEARNW.CPY".
       COPY "LIBGB/GETENVW.CPY".

      ******************************************************************
      *
      *   M I S C E L L A N E O U S - W O R K E R S
      *
      ******************************************************************
       01  DETAIL-LINE                 PIC X(66)     VALUE SPACES.
       01  D01-LINE                    REDEFINES DETAIL-LINE.
           03  D01-PROMPT              PIC X(16).
           03  D01-INFO                PIC X(50).

       01  DISPLAY-BUF                 VALUE SPACES.
           03  F010                    PIC X(66).
           03  F020                    PIC X(66).
           03  F030                    PIC X(66).
           03  F040                    PIC X(66).

       01  WS-LM-LTOUCH-TIME         PIC 9(8).
       01  FILLER REDEFINES WS-LM-LTOUCH-TIME.
           03  WS-LM-LTOUCH-HHMM     PIC 9(4).
           03  FILLER                PIC 9(4).
       01  WS-MISC                     PIC X(30).
       01  WS-START-TIME               PIC 9(6).
       01  WS-END-TIME                 PIC 9(6).
       01  WS-REC-CTR                  PIC 9(9)    VALUE 0.
       01  WS-SELECT                   PIC X(7100).
       01  MISC-CONN-OPEN-SW           PIC X     VALUE "N".
       01  WS-SUB                      PIC 9(9)  VALUE 0.
       01  WS-EOF                      PIC X     VALUE "N".

       01  PARM-BRNO                   PIC X(4).
       01  PARM-BRNO-RDF               REDEFINES PARM-BRNO
                                       PIC 9(4).

       01  TEST-BRANCH-X10             PIC X(10) VALUE SPACES.

       EXEC SQL BEGIN DECLARE SECTION END-EXEC.
       01  QWS-INCLUDE-PAIDOUTS        PIC X        VALUE SPACE.
       01  WS-CTR1                     PIC 9(9).
       01  WS-CTR2                     PIC 9(9).
       01  WS-ID                       PIC 9(9).
       01  WS-BRANCH                   PIC 9(4).
       01  WS-ACCTNO                   PIC 9(6).
       01  WS-CLASS                    PIC 9(3).
       01  MISC-RETURN-CODE            PIC S9(4)   COMP-5.
       01  WS-GR-ID                    PIC X(4).
       01  PASS-SQL                    PIC X(7500).
       01  WS-UPDATE-VAULE             PIC X.
       01  WS-MISC10                   PIC X(10).
       01  QWS-HOLD-LN-ACCTNO          PIC 9(8).
       01  QWS-HOLD-LN-LOANDATE        PIC X(10)    VALUE SPACES.   
       EXEC SQL END DECLARE SECTION END-EXEC.

       EXEC SQL BEGIN DECLARE SECTION END-EXEC.            
       01  QWS-PL               PIC X        VALUE SPACE.
       01  QWS-SERVA            PIC X        VALUE SPACE.
       01  QWS-ZBAL             PIC X        VALUE SPACE.
       01  QWS-SALEFIN          PIC X        VALUE SPACE.
       01  QWS-BULK             PIC X        VALUE SPACE.
       01  QWS-REPO             PIC X        VALUE SPACE.
       01  QWS-SEC-SECURED      PIC X        VALUE SPACE.
       01  QWS-SEC-UNSECURED    PIC X        VALUE SPACE. 
       01  QWS-SEC-REAL-ESTATE  PIC X        VALUE SPACE.

       EXEC SQL END DECLARE SECTION END-EXEC.

       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBGB/AGEINGW.CPY".

      * MUST BE LAST
       COPY "LIBGB/GETFMW.CPY".

      ******************************************************************
      *
      *    P R O C E D U R E - D I V I S I O N
      *
      ******************************************************************
       PROCEDURE DIVISION CHAINING PARM-BRNO.

       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
               PR-FILE .
       COPY "LIBGB/DECLRP.CPY".
       COPY "LIBGB/DECLAR2.CPY".

      ******************************************************************
      *
      *    M A I N - P R O G R A M
      *
      ******************************************************************
       MAIN-PROGRAM SECTION.

      D    STOP WS-BRANCH.
           ACCEPT WS-LM-LTOUCH-TIME   FROM TIME.

           MOVE "REL" TO GETENV-BUF.
           PERFORM GETENV-CALL.

      * NEED TO GET EXTERNAL VARIABLES SET
           MOVE GETENV-BUF TO FORM-REL.
           MOVE "CO" TO FORM-OBJ.
           MOVE "GB/SETENV" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH.
           CANCEL FORM-PROGX.

           PERFORM SQL-CONNECT.

           MOVE SPACES TO TEST-BRANCH-X10.
           MOVE TEST-BRANCH-X10 TO WS-BRANCH.
      D    STOP DISPLAY-BUF.

           PERFORM WRITE-MARK.

           GO TO MAIN-PROGAM-EXIT.

           PERFORM TEST-QUERY.
 
      *    PERFORM CREATE-TMP-TBL.
      *    IF SQLCODE NOT = 0
      *       GO TO MAIN-PROGAM-EXIT.
      *    PERFORM DROP-TMP-TBL.
      *    IF SQLCODE NOT = 0
      *       GO TO MAIN-PROGAM-EXIT.
      *    PERFORM WRITE-TMP-TBL.
      *    IF SQLCODE NOT = 0
      *       GO TO MAIN-PROGAM-EXIT.
      *    PERFORM READ-TMP-TBL.
      *    IF SQLCODE NOT = 0
      *       GO TO MAIN-PROGAM-EXIT.

      *     PERFORM DROP-TMP-TBL.

           PERFORM OPEN-MISC-CONNECTION.
           PERFORM SQL-OPEN-UPDATE-CONNECTION.

      *     PERFORM START-LN-EMB-CSR.
      *     PERFORM START-LN-CSR-SSP.

           MOVE "N" TO QWS-INCLUDE-PAIDOUTS.
      *     MOVE 811  TO QLN3-WBEG-OWNBR.
           MOVE PARM-BRNO-RDF TO QLN3-WBEG-OWNBR.

           PERFORM ARRAYSP-LOAD.

      *     MOVE 1504 TO QLN3-WBEG-OWNBR. 
      *     MOVE 0    TO QLN3-WBEG-CLASS.
      *     MOVE 999  TO QLN3-WEND-CLASS
      *     MOVE "I"  TO QWS-PL
      *                  QWS-ZBAL.
      *     MOVE 'S'  TO QWS-SERVA
      *                  QWS-SALEFIN
      *                  QWS-BULK
      *                  QWS-REPO.
      *     MOVE 'S'  TO QWS-SEC-SECURED.
      *     MOVE 'U'  TO QWS-SEC-UNSECURED.
      *     MOVE 'R'  TO QWS-SEC-REAL-ESTATE.

           MOVE 0   TO WS-REC-CTR.

           PERFORM GET-TIME.
           MOVE TIME-NOW TO WS-START-TIME.

      *    PERFORM VARYING WS-SUB FROM 1 BY 1 
      *     UNTIL WS-SUB > 10
      *       PERFORM START-LN3-FILE
      *          MOVE "N" TO WS-EOF
      *        MOVE 0   TO WS-REC-CTR
      *          PERFORM UNTIL WS-EOF = "Y"
      *             PERFORM READ-LN3-FILE-NEXT-ONE
      *             IF WS-EOF = "N"
      *                ADD 1 TO WS-REC-CTR
      *             END-IF
      *          END-PERFORM
      *       PERFORM CLOSE-LN3-CSR
      *    END-PERFORM.
      *
      *    PERFORM GET-TIME.
      *    MOVE TIME-NOW TO WS-END-TIME.
      *    STRING WS-START-TIME, " ", WS-END-TIME, " ", WS-REC-CTR 
      *        INTO WS-MISC.

           MOVE 20240630 TO AGEING-DATE.

           PERFORM GET-TIME.
           MOVE TIME-NOW TO WS-START-TIME.

           PERFORM START-LN3C-FILE.
           PERFORM UNTIL SQLCODE = +100
              PERFORM READ-LN3C-FILE-NEXT
              IF SQLCODE NOT = +0
                 EXIT PERFORM
              END-IF
              ADD 1 TO WS-REC-CTR
      *       MOVE LN-ORGST    TO SP-ORGST
      *       MOVE LN-SPRCLASS TO SP-SPRCLASS
      *       MOVE LN-SUBCLASS TO SP-SUBCLASS
      *       MOVE LN-LAWCODE  TO SP-LAWCODE
      *       IF LPWSSP-SP1-KEY NOT = SP1-KEY
      *          PERFORM ARRAYSP-READ
      *          MOVE SP1-KEY TO LPWSSP-SP1-KEY
      *       END-IF
      *       PERFORM COMPUTE-CONTRACTUAL
      *       PERFORM COMPUTE-CONTRACTUAL
      *     PERFORM COMPUTE-RECENCY
      *     PERFORM COMPUTE-RECENCY
      *     PERFORM COMPUTE-EARNINGS
      *     PERFORM REWRITE-LN3-FILE
           END-PERFORM.
           PERFORM CLOSE-LN3C-FILE.

           PERFORM GET-TIME.
           MOVE TIME-NOW TO WS-END-TIME.
           STRING WS-START-TIME, " ", WS-END-TIME, " ", WS-REC-CTR 
              INTO WS-MISC.
      D    STOP WS-MISC.

      *     PERFORM READ-LN-CSR-NEXT.

      *     MOVE " " TO WS-UPDATE-VAULE.
      *     PERFORM VARYING WS-SUB FROM 1 BY 1 UNTIL WS-SUB > 500000
      *        PERFORM READ-LN-CSR-NEXT-SSP
      *        IF SQLCODE NOT = 0
      *          GO TO MAIN-PROGAM-EXIT
      *        END-IF
      *        PERFORM UPDATE-LN-CSR-ROW-SSP
      *        IF SQLCODE NOT = 0
      *           GO TO MAIN-PROGAM-EXIT
      *        END-IF
      *     END-PERFORM.

      *     PERFORM CLOSE-LN-CSR.
      *     PERFORM CLOSE-LN-CSR-SSP.

           PERFORM SQL-CLOSE-UPDATE-CONNECTION.
           PERFORM CLOSE-MISC-CONNECTION.

      *=================================================================
       MAIN-PROGAM-EXIT.
           GO TO END-PROGRAM.

      *-----------------------------------------------------------------
       WRITE-MARK.
           MOVE "MBSQL EMBEDDED CSR" TO E-FILE.
           MOVE SPACES               TO E-KEYX.
           MOVE "WRITE ERROR"        TO E-MSG.
           MOVE "UT/MBSQLT"          TO E-PROG.

           EXEC SQL
            INSERT INTO DBO.MARK 
              VALUES  (:WS-BRANCH,
                       'ABC')
           END-EXEC.

           IF SQLCODE NOT = 0
             PERFORM SQL-ERROR.

      *-----------------------------------------------------------------
       TEST-QUERY.

           MOVE 1            TO QWS-HOLD-LN-ACCTNO.
           MOVE "2049-01-01" TO QWS-HOLD-LN-LOANDATE.

           EXEC SQL
            SELECT CAST(MAX(LNFILE.LN_LOANDATE) AS VARCHAR(10))
             INTO :WS-MISC10
             FROM DBO.LNFILE
           INNER JOIN DBO.BRFILE BRFILE
               ON  ( BRFILE.BR_NO               = LNFILE.LN_OWNBR
              AND    BRFILE.BR_ADD_INS_FEECODE <> '  ' )
            WHERE LNFILE.LN_OWNBR  = 137
              AND   LNFILE.LN_ACCTNO  <> :QWS-HOLD-LN-ACCTNO
              AND LNFILE.LN_LOANDATE  <= :QWS-HOLD-LN-LOANDATE
           END-EXEC.   

      *-----------------------------------------------------------------
       START-LN3C-FILE. 
           MOVE "MBSQL EMBEDDED CSR" TO E-FILE.
           MOVE SPACES               TO E-KEYX.
           MOVE "START ERROR"        TO E-MSG.
           MOVE "UT/MBSQLT"          TO E-PROG.

           EXEC SQL
            DECLARE LN3C_CURSOR CURSOR FOR
                :LN3-RETURN-CODE = 
                 EXEC SSP_EOMALL_LNFILE_SELECT
                  ( :QLN3-WBEG-OWNBR)
                END-EXEC.

           EXEC SQL
               OPEN LN3C_CURSOR
           END-EXEC.

           IF SQLCODE NOT = 0
             PERFORM SQL-ERROR.

      *-----------------------------------------------------------------
       READ-LN3C-FILE-NEXT.   *> SSP - LN 
           MOVE "MBSQL EMBEDDED CSR" TO E-FILE.
           MOVE SPACES               TO E-KEYX.
           MOVE "READ ERROR"        TO E-MSG.
           MOVE "UT/MBSQLT"          TO E-PROG.
           INITIALIZE QLN-REC.

           EXEC SQL
                  FETCH LN3C_CURSOR INTO 
                   :QLN-OWNBR,
                   :QLN-ACCTNO,
                   :QLN-CLASS,
                   :QLN-SUBCLASS,
                   :QLN-LAWCODE,
                   :QLN-ORGST,
                   :QLN-SPRCLASS,
                   :QLN-SRCD,
                   :QLN-POCD,
                   :QLN-PLCD,
                   :QLN-PLTYPE,
                   :QLN-SERVACCT,
                   :QLN-PAY-SCHLD-FG,
                   :QLN-LOANDATE,
                   :QLN-ORIG-1STPYDATE, 
                   :QLN-1STPYDATE,
                   :QLN-PLDATE,     
                   :QLN-POFFDATE,
                   :QLN-BNKRPTDATE,
                   :QLN-REPODATE,
                   :QLN-REDEEMDATE,
                   :QLN-TRW-DOFD-DATE,
                   :QLN-LNAMT,
                   :QLN-ORGTERM,
                   :QLN-1STPYAMT,
                   :QLN-NOREGPY,
                   :QLN-REGPYAMT,
                   :QLN-LASTPYAMT,
                   :QLN-PY-YYYYMM-RECDEL,
                   :QLN-MTD-PAYS-RECDEL,
                   :QLN-DLNO,
                   :QLN-CURBAL,
                   :QLN-INTBAL,
                   :QLN-LCBAL,
                   :QLN-OTHBAL,
                   :QLN-OT2BAL,
                   :QLN-MONTHEND-BAL,
                   :QLN-ANTICERN-1,
                   :QLN-ANTICERN-2,
                   :QLN-ANTICERN-3,
                   :QLN-ANTICERN-4,
                   :QLN-ANTICERN-5,
                   :QLN-ANTICERN-6,
                   :QLN-ANTICERN-7,
                   :QLN-ANTICERN-8,
                   :QLN-ANTICERN-9,
                   :QLN-ANTICERN-10,
                   :QLN-ANTICADJ-1,
                   :QLN-ANTICADJ-2,
                   :QLN-ANTICADJ-3,
                   :QLN-ANTICADJ-4,
                   :QLN-ANTICADJ-5,
                   :QLN-ANTICADJ-6,
                   :QLN-ANTICADJ-7,
                   :QLN-ANTICADJ-8,
                   :QLN-ANTICADJ-9,
                   :QLN-ANTICADJ-10,
                   :QLN-UFERN-1,
                   :QLN-UFERN-2,
                   :QLN-UFERN-3,
                   :QLN-UFERN-4,
                   :QLN-UFERN-5,
                   :QLN-UFERN-6,
                   :QLN-UFERN-7,
                   :QLN-UFERN-8,
                   :QLN-UFERN-9,
                   :QLN-UFERN-10,
                   :QLN-ACCERN-1,
                   :QLN-ACCERN-2,
                   :QLN-ACCERN-3,
                   :QLN-ACCERN-4,
                   :QLN-ACCERN-5,
                   :QLN-ACCERN-6,
                   :QLN-ACCERN-7,
                   :QLN-ACCERN-8,
                   :QLN-ACCERN-9,
                   :QLN-ACCERN-10,
                   :QLN-TOTPAYMNTD,
                   :QLN-TOTNODEF,
                   :QLN-YTDNODEF,
                   :QLN-YTDNODEFACT,
                   :QLN-TOTNOALDEL,
                   :QLN-YTDNOALDEL,
                   :QLN-DATE-PAID-LAST,
                   :QLN-PRLNAMT,
                   :QLN-PRPDUE-DAYS,
                   :QLN-SUM-ORGTERM,
                   :QLN-UNITPER-CD,
                   :QLN-UNITPER-FREQ,
                   :QLN-REPOCD,
                   :QLN-MTD-PRIN,
                   :QLN-MTD-LC,
                   :QLN-MTD-INT,
                   :QLN-MTD-OT,
                   :QLN-MTD-EX,
                   :QLN-BMO-CON-STATUS,
                   :QLN-BMO-REC-STATUS,
                   :QLN-CONPROFILE-1,
                   :QLN-CONPROFILE-2,
                   :QLN-CONPROFILE-3,
                   :QLN-CONPROFILE-4,
                   :QLN-CONPROFILE-5,
                   :QLN-RECPROFILE-1,
                   :QLN-RECPROFILE-2,
                   :QLN-RECPROFILE-3,
                   :QLN-RECPROFILE-4,
                   :QLN-RECPROFILE-5
              END-EXEC.

           IF SQLCODE = +0
              PERFORM GET-LN-FIELDS
              NEXT SENTENCE
           ELSE 
             IF SQLCODE NOT = +100
             PERFORM SQL-ERROR.

      *-----------------------------------------------------------------
       REWRITE-LN3-FILE.

           MOVE "MBSQL EMBEDDED CSR" TO E-FILE.
           MOVE SPACES               TO E-KEYX.
           MOVE "REWRITE ERROR"      TO E-MSG.
           MOVE "UT/MBSQLT"          TO E-PROG.

           ACCEPT LN-LTOUCH-DATE FROM CENTURY-DATE.

           PERFORM SET-LN-FIELDS.

           MOVE "2" TO SQL-UPDATE-CONN-USE-SW.    *> CHANGE CONNECTION UPDATE_CONNECT
           PERFORM SQL-SWAP-UPDATE-CONNECTION.

           EXEC SQL
            UPDATE DBO.LNFILE
             SET   LNFILE.LN_TRW_DOFD_DATE    = :QLN-TRW-DOFD-DATE,
                   LNFILE.LN_PY_YYYYMM_RECDEL = :QLN-PY-YYYYMM-RECDEL,
                   LNFILE.LN_MTD_PAYS_RECDEL  = :QLN-MTD-PAYS-RECDEL,
                   LNFILE.LN_MONTHEND_BAL     = :QLN-MONTHEND-BAL,
                   LNFILE.LN_MTD_PRIN         = :QLN-MTD-PRIN,
                   LNFILE.LN_MTD_LC           = :QLN-MTD-LC,
                   LNFILE.LN_MTD_INT          = :QLN-MTD-INT,
                   LNFILE.LN_MTD_OT           = :QLN-MTD-OT,
                   LNFILE.LN_MTD_EX           = :QLN-MTD-EX,
                   LNFILE.LN_BMO_CON_STATUS   = :QLN-BMO-CON-STATUS,
                   LNFILE.LN_BMO_REC_STATUS   = :QLN-BMO-REC-STATUS,
                   LNFILE.LN_CONPROFILE_1     = :QLN-CONPROFILE-1,
                   LNFILE.LN_CONPROFILE_2     = :QLN-CONPROFILE-2,
                   LNFILE.LN_CONPROFILE_3     = :QLN-CONPROFILE-3,
                   LNFILE.LN_CONPROFILE_4     = :QLN-CONPROFILE-4,
                   LNFILE.LN_CONPROFILE_5     = :QLN-CONPROFILE-5,
                   LNFILE.LN_RECPROFILE_1     = :QLN-RECPROFILE-1,
                   LNFILE.LN_RECPROFILE_2     = :QLN-RECPROFILE-2,
                   LNFILE.LN_RECPROFILE_3     = :QLN-RECPROFILE-3,
                   LNFILE.LN_RECPROFILE_4     = :QLN-RECPROFILE-4,
                   LNFILE.LN_RECPROFILE_5     = :QLN-RECPROFILE-5,
                   LNFILE.LN_LTOUCH_DATE      = :QLN-LTOUCH-DATE
             WHERE LNFILE.LN_OWNBR  = :QLN-OWNBR
               AND LNFILE.LN_ACCTNO = :QLN-ACCTNO
           END-EXEC.

           IF SQLCODE NOT = +0
             PERFORM SQL-ERROR.

           MOVE "1" TO SQL-UPDATE-CONN-USE-SW.    *> CHANGE CONNECTION BACK TO C1
           PERFORM SQL-SWAP-UPDATE-CONNECTION.

      *-----------------------------------------------------------------
       CLOSE-LN3C-FILE.

              EXEC SQL
                   CLOSE LN3C_CURSOR
              END-EXEC.

      *-----------------------------------------------------------------
       READ-TBL-TEST2.
           MOVE "MBSQL EMBEDDED CSR" TO E-FILE.
           MOVE SPACES               TO E-KEYX.
           MOVE "READ ERROR"        TO E-MSG.
           MOVE "UT/MBSQLT"          TO E-PROG.

           EXEC SQL
               SELECT SUM(CASE WHEN LNFILE.LN_POCD IN ('*I','*R')
                         THEN 1 ELSE 0 END)
                 ,SUM(CASE WHEN LNFILE.LN_POCD IN ('*I','*R') THEN 0
                   WHEN LNFILE.LN_POCD = 'BT' 
                     OR LNFILE.LN_SKIP_TRW = 'Y' THEN 1
                   WHEN LNFILE.LN_CONVERCD = 'Y'
                     AND LNFILE.LN_ENTDATE < '2019-01-01' THEN 1
                   WHEN LNFILE.LN_CURBAL = 0 
                     AND LNFILE.LN_POFFDATE = '1900-01-01' THEN 1
                   ELSE 0 END)
               INTO :WS-CTR1, :WS-CTR2 
                  FROM DBO.LNFILE LNFILE
                  WHERE LNFILE.LN_OWNBR = 1504
                  AND LNFILE.LN_CLASS  BETWEEN 0 AND 999
                  AND LNFILE.LN_ACCTNO BETWEEN 0 AND 99999999
           END-EXEC.

           IF SQLCODE NOT = 0
             PERFORM SQL-ERROR.

      *-----------------------------------------------------------------
       READ-TBL-TEST.
           MOVE "MBSQL EMBEDDED CSR" TO E-FILE.
           MOVE SPACES               TO E-KEYX.
           MOVE "READ ERROR"        TO E-MSG.
           MOVE "UT/MBSQLT"          TO E-PROG.

           EXEC SQL
            SELECT LPFILE.LP_BRNO,
                   LPFILE.LP_ACCTNO,
                   LPFILE.LP_SEQNO,
                   CAST(LPFILE.LP_TRDATE AS VARCHAR(10)),
                   LPFILE.LP_TRCD,
                   LPFILE.LP_CURBAL
            INTO       :QLP-BRNO,
                       :QLP-ACCTNO,
                       :QLP-SEQNO,
                       :QLP-TRDATE,
                       :QLP-TRCD,
                       :QLP-CURBAL

              FROM DBO.LPFILE LPFILE
             WHERE LPFILE.LP_BRNO    = 1504
               AND LPFILE.LP_ACCTNO  = 001007
               AND LPFILE.LP_SEQNO  =
              (SELECT MAX(LPFILE.LP_SEQNO)
                FROM DBO.LPFILE LPFILE
                WHERE LPFILE.LP_BRNO   = 1504
                 AND LPFILE.LP_ACCTNO  = 001007)
           END-EXEC.

           IF SQLCODE NOT = 0
             PERFORM SQL-ERROR.

      *-----------------------------------------------------------------
       START-LN3-FILE. *> SSP - LN,BW,NO

           EXEC SQL SET CONNECTION MISC_CONNECT END-EXEC.

          EXEC SQL
           DECLARE LN3_CURSOR CURSOR FOR
               :MISC-RETURN-CODE = EXEC SSP_CRNOFL_LNFILE_SELECT 
             ( :QLN3-WBEG-OWNBR,
               :QWS-INCLUDE-PAIDOUTS )
          END-EXEC.
      

      *    EXEC SQL
      *     DECLARE LN3_CURSOR CURSOR FOR
      *         :MISC-RETURN-CODE = EXEC SSP_BNKRP1_LNFILE_SELECT 
      *       ( :QLN3-WBEG-OWNBR, 
      *         :QLN3-WBEG-CLASS, 
      *         :QLN3-WEND-CLASS,
      *         :QWS-PL,
      *         :QWS-SERVA,
      *         :QWS-ZBAL,
      *         :QWS-SALEFIN,
      *         :QWS-BULK,
      *         :QWS-REPO,
      *         :QWS-SEC-SECURED,
      *         :QWS-SEC-UNSECURED,
      *         :QWS-SEC-REAL-ESTATE  )
      *    END-EXEC.
 
           EXEC SQL
               OPEN LN3_CURSOR
           END-EXEC.

           IF SQLCODE NOT = 0
             PERFORM SQL-ERROR.

           EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
       READ-LN3-FILE-NEXT-ONE.

           INITIALIZE QLN-REC.
           INITIALIZE QBW-REC.
           INITIALIZE QLC-REC.
           INITIALIZE QCD-REC.

           EXEC SQL SET CONNECTION MISC_CONNECT END-EXEC.

           EXEC SQL
             FETCH LN3_CURSOR 
                 INTO 
                     :QLN-OWNBR
              END-EXEC.

           IF SQLCODE = 100
              MOVE "Y" TO WS-EOF
           ELSE 
              IF SQLCODE NOT = 0
                 PERFORM SQL-ERROR.
 
           EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
       READ-LN3-FILE-NEXT-ALL.

           INITIALIZE QLN-REC.
           INITIALIZE QBW-REC.
           INITIALIZE QLC-REC.
           INITIALIZE QCD-REC.

           EXEC SQL SET CONNECTION MISC_CONNECT END-EXEC.

           EXEC SQL
             FETCH LN3_CURSOR 
                 INTO 
                     :QLN-OWNBR, 
                     :QLN-ACCTNO,
                     :QLN-CLASS,
                     :QLN-SUBCLASS,
                     :QLN-LAWCODE,
                     :QLN-SSNO1,
                     :QLN-MAKERCD1,
                     :QLN-SSNO2,
                     :QLN-MAKERCD2,
                     :QLN-SSNO3,
                     :QLN-MAKERCD3,
                     :QLN-SSNO4,
                     :QLN-MAKERCD4,
                     :QLN-ORGST,
                     :QLN-ORGBR,
                     :QLN-SPRCLASS,
                     :QLN-CONVERCD, 
                     :QLN-MIDSTREAM, 
                     :QLN-PURCD, 
                     :QLN-SRCD, 
                     :QLN-COLLCD, 
                     :QLN-POCD, 
                     :QLN-PLCD, 
                     :QLN-STATUSFG, 
                     :QLN-SALEFIN, 
                     :QLN-SERVACCT, 
                     :QLN-LOANTYPE, 
                     :QLN-ADDON-FG, 
                     :QLN-PAY-SCHLD-FG, 
                     :QLN-STATFLAGS, 
                     :QLN-ENTDATE,
                     :QLN-LOANDATE,
                     :QLN-PURDATE,
                     :QLN-INTDATE,
                     :QLN-CASHDATE,
                     :QLN-RESCISSION-DATE,
                     :QLN-ORIG-1STPYDATE,
                     :QLN-1STPYDATE,
                     :QLN-JDDATE,
                     :QLN-PLDATE,
                     :QLN-ACCELDATE,
                     :QLN-REDUDATE,
                     :QLN-APRATE,
                     :QLN-EFFRATE,
                     :QLN-SMPRATE,
                     :QLN-OVRDRATE,
                     :QLN-JDRATE,
                     :QLN-ACCRATE,
                     :QLN-LNAMT,
                     :QLN-FINCHG,
                     :QLN-INTCHG,
                     :QLN-SERCHG,
                     :QLN-EXTCHG,
                     :QLN-MAINTFEE,
                     :QLN-POINTS,
                     :QLN-PROCEEDS,
                     :QLN-ORGTERM,
                     :QLN-1STPYAMT,
                     :QLN-NOREGPY,
                     :QLN-REGPYAMT,
                     :QLN-LASTPYAMT,
                     :QLN-FEECODE-1, 
                     :QLN-FEECODE-2, 
                     :QLN-FEECODE-3, 
                     :QLN-FEECODE-4, 
                     :QLN-FEECODE-5, 
                     :QLN-FEECODE-6, 
                     :QLN-FEECODE-7, 
                     :QLN-FEECODE-8, 
                     :QLN-FEECODE-9, 
                     :QLN-FEECODE-10, 
                     :QLN-FEECODE-11,
                     :QLN-FEECODE-12,
                     :QLN-FEECODE-13,
                     :QLN-FEECODE-14,
                     :QLN-FEECODE-15,
                     :QLN-FEECODE-16,
                     :QLN-FEECODE-17,
                     :QLN-FEECODE-18,
                     :QLN-FEECODE-19,
                     :QLN-FEECODE-20,
                     :QLN-FEEAMT-1,
                     :QLN-FEEAMT-2,
                     :QLN-FEEAMT-3,
                     :QLN-FEEAMT-4,
                     :QLN-FEEAMT-5,
                     :QLN-FEEAMT-6,
                     :QLN-FEEAMT-7,
                     :QLN-FEEAMT-8,
                     :QLN-FEEAMT-9,
                     :QLN-FEEAMT-10,
                     :QLN-FEEAMT-11,
                     :QLN-FEEAMT-12,
                     :QLN-FEEAMT-13,
                     :QLN-FEEAMT-14,
                     :QLN-FEEAMT-15,
                     :QLN-FEEAMT-16,
                     :QLN-FEEAMT-17,
                     :QLN-FEEAMT-18,
                     :QLN-FEEAMT-19,
                     :QLN-FEEAMT-20,
                     :QLN-FEEOURS-1,
                     :QLN-FEEOURS-2,
                     :QLN-FEEOURS-3,
                     :QLN-FEEOURS-4,
                     :QLN-FEEOURS-5,
                     :QLN-FEEOURS-6,
                     :QLN-FEEOURS-7,
                     :QLN-FEEOURS-8,
                     :QLN-FEEOURS-9,
                     :QLN-FEEOURS-10,
                     :QLN-FEEOURS-11,
                     :QLN-FEEOURS-12,
                     :QLN-FEEOURS-13,
                     :QLN-FEEOURS-14,
                     :QLN-FEEOURS-15,
                     :QLN-FEEOURS-16,
                     :QLN-FEEOURS-17,
                     :QLN-FEEOURS-18,
                     :QLN-FEEOURS-19,
                     :QLN-FEEOURS-20,
                     :QLN-FEEFINCHG-1,
                     :QLN-FEEFINCHG-2,
                     :QLN-FEEFINCHG-3,
                     :QLN-FEEFINCHG-4,
                     :QLN-FEEFINCHG-5,
                     :QLN-FEEFINCHG-6,
                     :QLN-FEEFINCHG-7,
                     :QLN-FEEFINCHG-8,
                     :QLN-FEEFINCHG-9,
                     :QLN-FEEFINCHG-10, 
                     :QLN-FEEFINCHG-11,
                     :QLN-FEEFINCHG-12,
                     :QLN-FEEFINCHG-13,
                     :QLN-FEEFINCHG-14,
                     :QLN-FEEFINCHG-15,
                     :QLN-FEEFINCHG-16,
                     :QLN-FEEFINCHG-17,
                     :QLN-FEEFINCHG-18,
                     :QLN-FEEFINCHG-19,
                     :QLN-FEEFINCHG-20, 
                     :QLN-INSURED, 
                     :QLN-INSTYPES, 
                     :QLN-INSCOMP-1,
                     :QLN-INSCOMP-2,
                     :QLN-INSCOMP-3,
                     :QLN-INSCOMP-4,
                     :QLN-INSCOMP-5,
                     :QLN-INSCOMP-6,
                     :QLN-INSCOMP-7,
                     :QLN-INSCOMP-8,
                     :QLN-INSCOMP-9,
                     :QLN-INSCOMP-10,
                     :QLN-INSOURS-1, 
                     :QLN-INSOURS-2, 
                     :QLN-INSOURS-3, 
                     :QLN-INSOURS-4, 
                     :QLN-INSOURS-5, 
                     :QLN-INSOURS-6, 
                     :QLN-INSOURS-7, 
                     :QLN-INSOURS-8, 
                     :QLN-INSOURS-9, 
                     :QLN-INSOURS-10,
                     :QLN-INSEFF-DATE-1,
                     :QLN-INSEFF-DATE-2,
                     :QLN-INSEFF-DATE-3,
                     :QLN-INSEFF-DATE-4,
                     :QLN-INSEFF-DATE-5,
                     :QLN-INSEFF-DATE-6,
                     :QLN-INSEFF-DATE-7,
                     :QLN-INSEFF-DATE-8,
                     :QLN-INSEFF-DATE-9,
                     :QLN-INSEFF-DATE-10,
                     :QLN-INSEXP-DATE-1,
                     :QLN-INSEXP-DATE-2,
                     :QLN-INSEXP-DATE-3,
                     :QLN-INSEXP-DATE-4,
                     :QLN-INSEXP-DATE-5,
                     :QLN-INSEXP-DATE-6,
                     :QLN-INSEXP-DATE-7,
                     :QLN-INSEXP-DATE-8,
                     :QLN-INSEXP-DATE-9,
                     :QLN-INSEXP-DATE-10,
                     :QLN-INSPREM-1,
                     :QLN-INSPREM-2,
                     :QLN-INSPREM-3,
                     :QLN-INSPREM-4,
                     :QLN-INSPREM-5,
                     :QLN-INSPREM-6,
                     :QLN-INSPREM-7,
                     :QLN-INSPREM-8,
                     :QLN-INSPREM-9,
                     :QLN-INSPREM-10,
                     :QLN-INSCOVR-1,
                     :QLN-INSCOVR-2,
                     :QLN-INSCOVR-3,
                     :QLN-INSCOVR-4,
                     :QLN-INSCOVR-5,
                     :QLN-INSCOVR-6,
                     :QLN-INSCOVR-7,
                     :QLN-INSCOVR-8,
                     :QLN-INSCOVR-9,
                     :QLN-INSCOVR-10,
                     :QLN-INSFINCHG-1, 
                     :QLN-INSFINCHG-2, 
                     :QLN-INSFINCHG-3, 
                     :QLN-INSFINCHG-4, 
                     :QLN-INSFINCHG-5, 
                     :QLN-INSFINCHG-6, 
                     :QLN-INSFINCHG-7, 
                     :QLN-INSFINCHG-8, 
                     :QLN-INSFINCHG-9, 
                     :QLN-INSFINCHG-10, 
                     :QLN-RESCISSION, 
                     :QLN-CURBAL,
                     :QLN-INTBAL,
                     :QLN-LCBAL,
                     :QLN-OTHBAL,
                     :QLN-OT2BAL,
                     :QLN-ANTICERN-1,
                     :QLN-ANTICERN-2,
                     :QLN-ANTICERN-3,
                     :QLN-ANTICERN-4,
                     :QLN-ANTICERN-5,
                     :QLN-ANTICERN-6,
                     :QLN-ANTICERN-7,
                     :QLN-ANTICERN-8,
                     :QLN-ANTICERN-9,
                     :QLN-ANTICERN-10,
                     :QLN-ANTICADJ-1,
                     :QLN-ANTICADJ-2,
                     :QLN-ANTICADJ-3,
                     :QLN-ANTICADJ-4,
                     :QLN-ANTICADJ-5,
                     :QLN-ANTICADJ-6,
                     :QLN-ANTICADJ-7,
                     :QLN-ANTICADJ-8,
                     :QLN-ANTICADJ-9,
                     :QLN-ANTICADJ-10,
                     :QLN-UFERN-1,
                     :QLN-UFERN-2,
                     :QLN-UFERN-3,
                     :QLN-UFERN-4,
                     :QLN-UFERN-5,
                     :QLN-UFERN-6,
                     :QLN-UFERN-7,
                     :QLN-UFERN-8,
                     :QLN-UFERN-9,
                     :QLN-UFERN-10,
                     :QLN-ACCERN-1,
                     :QLN-ACCERN-2,
                     :QLN-ACCERN-3,
                     :QLN-ACCERN-4,
                     :QLN-ACCERN-5,
                     :QLN-ACCERN-6,
                     :QLN-ACCERN-7,
                     :QLN-ACCERN-8,
                     :QLN-ACCERN-9,
                     :QLN-ACCERN-10,
                     :QLN-TOTINT,
                     :QLN-TOTLCHG,
                     :QLN-TOTDEF,
                     :QLN-TOTPAYMNTD,
                     :QLN-TOTNODEF,
                     :QLN-TOTNOALDEL,
                     :QLN-CL-REBATE, 
                     :QLN-AH-REBATE, 
                     :QLN-PP-REBATE, 
                     :QLN-INT-REBATE, 
                     :QLN-SC-REBATE, 
                     :QLN-MF-REBATE, 
                     :QLN-DF-REBATE, 
                     :QLN-EX-REBATE, 
                     :QLN-O1-REBATE, 
                     :QLN-O2-REBATE, 
                     :QLN-O3-REBATE, 
                     :QLN-O4-REBATE, 
                     :QLN-O5-REBATE, 
                     :QLN-O6-REBATE, 
                     :QLN-O7-REBATE, 
                     :QLN-UN-O7-REB, 
                     :QLN-UN-O8-REB, 
                     :QLN-UN-O9-REB, 
                     :QLN-UN-O10-REB, 
                     :QLN-DATE-PAID-LAST,
                     :QLN-CREDITLIM,
                     :QLN-OLDACCTNO,
                     :QLN-PRLNNO,
                     :QLN-PRBAL,
                     :QLN-PRINTDUE,
                     :QLN-PRFINCHGRB,
                     :QLN-PRINSRB,
                     :QLN-SUM-FINCHG,
                     :QLN-SUM-ORGTERM,
                     :QLN-UNITPER-CD, 
                     :QLN-UNITPER-FREQ,
                     :QLN-REPOCD, 
                     :QLN-BR-TFR-FG, 
                     :QBW-OWNBR,
                     :QBW-SSNO,
                     :QBW-LNAME,
                     :QBW-FNAME,
                     :QBW-SOLICIT, 
                     :QLC-CODE,
                     :QLC-DESC,
                     :QCD-TYPE,
                     :QCD-CODE,
                     :QCD-DESC,
                     :QCD-CO-SECURED
              END-EXEC.

           IF SQLCODE = 100
              MOVE "Y" TO WS-EOF
           ELSE 
              IF SQLCODE NOT = 0
                 PERFORM SQL-ERROR.
 
           EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------

      ******************************************************************
       OPEN-MISC-CONNECTION SECTION.
      ******************************************************************

           IF MISC-CONN-OPEN-SW = "N" 
               EXEC SQL CONNECT TO :SQL-CONNECT-SERVER AS MISC_CONNECT
                    USER :SQL-CONNECT-USERID USING :SQL-CONNECT-PSWD
               END-EXEC
               MOVE "Y" TO MISC-CONN-OPEN-SW
               EXEC SQL SET CONNECTION C1 END-EXEC
               PERFORM SQL-IO-VALIDATION.

      ******************************************************************
       CLOSE-MISC-CONNECTION SECTION.
      ******************************************************************

           IF MISC-CONN-OPEN-SW = "Y"
               EXEC SQL SET CONNECTION MISC_CONNECT END-EXEC
               EXEC SQL COMMIT END-EXEC
               EXEC SQL DISCONNECT MISC_CONNECT END-EXEC
               EXEC SQL SET CONNECTION C1 END-EXEC
               MOVE "N" TO MISC-CONN-OPEN-SW.

      ****************************************
       IO-ROUTINES SECTION.
      ****************************************
       COPY "LIBGB/CONNECT_SQL.CPY".


      *-----------------------------------------------------------------
       CREATE-TMP-TBL.
           MOVE "MBSQL EMBEDDED CSR" TO E-FILE.
           MOVE SPACES               TO E-KEYX.
           MOVE "CREATE ERROR"       TO E-MSG.
           MOVE "UT/MBSQLT"          TO E-PROG.

           EXEC SQL
            CREATE TABLE #TEMP1_TEST_TBL
              ( BR_NO   INTEGER);
           END-EXEC.

           IF SQLCODE NOT = 0
             PERFORM SQL-ERROR.

      *-----------------------------------------------------------------
       WRITE-TMP-TBL.
           MOVE "MBSQL EMBEDDED CSR" TO E-FILE.
           MOVE SPACES               TO E-KEYX.
           MOVE "WRITE ERROR"        TO E-MSG.
           MOVE "UT/MBSQLT"          TO E-PROG.

           EXEC SQL
            INSERT INTO #TEMP1_TEST_TBL
              VALUES  (1234) 
           END-EXEC.

           IF SQLCODE NOT = 0
             PERFORM SQL-ERROR.

      *-----------------------------------------------------------------
       READ-TMP-TBL.
           MOVE "MBSQL EMBEDDED CSR" TO E-FILE.
           MOVE SPACES               TO E-KEYX.
           MOVE "READ ERROR"        TO E-MSG.
           MOVE "UT/MBSQLT"          TO E-PROG.

           EXEC SQL
            SELECT BR_NO
            INTO :WS-BRANCH
            FROM #TEMP1_TEST_TBL
            WHERE BR_NO = 1234
           END-EXEC.

           IF SQLCODE NOT = 0
             PERFORM SQL-ERROR.

      *-----------------------------------------------------------------
       DROP-TMP-TBL.
           MOVE "MBSQL EMBEDDED CSR" TO E-FILE.
           MOVE SPACES               TO E-KEYX.
           MOVE "DROP ERROR"        TO E-MSG.
           MOVE "UT/MBSQLT"          TO E-PROG.

           EXEC SQL
            DROP TABLE #TEMP1_TEST_TBL;
           END-EXEC.

           IF SQLCODE NOT = 0
             PERFORM SQL-ERROR.


      *-----------------------------------------------------------------
       START-LN-EMB-CSR.
           MOVE "MBSQL EMBEDDED CSR" TO E-FILE.
           MOVE SPACES               TO E-KEYX.
           MOVE "START ERROR"        TO E-MSG.
           MOVE "UT/MBSQLT"          TO E-PROG.

      *      FOR UPDATE OF LN_UNWIND_REASON_CODE
      *     SELECT LNFILE.LN_OWNBR,
      *             LNFILE.LN_ACCTNO

           EXEC SQL
            DECLARE LNA_CURSOR CURSOR FOR
             SELECT *
             FROM DBO.LNFILE
             WHERE LNFILE.LN_OWNBR BETWEEN 11 AND 12
             ORDER BY LN_OWNBR, LN_ACCTNO
           END-EXEC.

           EXEC SQL
               OPEN LNA_CURSOR
           END-EXEC.

           IF SQLCODE NOT = 0
             PERFORM SQL-ERROR.

      *-----------------------------------------------------------------
       START-LN-CSR-SSP.
           MOVE "MBSQL EMBEDDED CSR" TO E-FILE.
           MOVE SPACES               TO E-KEYX.
           MOVE "START ERROR"        TO E-MSG.
           MOVE "UT/MBSQLT"          TO E-PROG.

           INITIALIZE WS-SELECT PASS-SQL.

           STRING "SELECT LNFILE.*"
                  " FROM DBO.BRFILE BRFILE"
                  " INNER JOIN DBO.LNFILE LNFILE"
                  "  ON LNFILE.LN_OWNBR = BRFILE.BR_NO"
                  " WHERE BRFILE.BR_NO BETWEEN 1504 AND 1504"
                  " ORDER BY LNFILE.LN_OWNBR, LNFILE.LN_ACCTNO | " 
              INTO WS-SELECT.
 
           STRING WS-SELECT DELIMITED BY "|" INTO PASS-SQL.

           INSPECT PASS-SQL REPLACING ALL "|" BY SPACE.

           EXEC SQL SET CONNECTION MISC_CONNECT END-EXEC.

           EXEC SQL
            DECLARE LNSSP_CURSOR CURSOR FOR
                :MISC-RETURN-CODE = 
                 EXEC SSP_TCLP_SELECT
                  ( :PASS-SQL)
                END-EXEC.

           EXEC SQL
               OPEN LNSSP_CURSOR 
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
       READ-LN-CSR-NEXT.
           MOVE "MBSQL CSR"        TO E-FILE.
           MOVE SPACES             TO E-KEYX.
           MOVE "READ NEXT ERROR"  TO E-MSG.
           MOVE "UT/MBSQLT"        TO E-PROG.

           EXEC SQL
               FETCH LNA_CURSOR INTO 
                 :WS-ID,
                 :WS-BRANCH,
                 :WS-ACCTNO
           END-EXEC.

           IF SQLCODE NOT = 0
             PERFORM SQL-ERROR.

      *-----------------------------------------------------------------
       READ-LN-CSR-NEXT-SSP.
           MOVE "MBSQL CSR"        TO E-FILE.
           MOVE SPACES             TO E-KEYX.
           MOVE "READ NEXT ERROR"  TO E-MSG.
           MOVE "UT/MBSQLT"        TO E-PROG.

           EXEC SQL SET CONNECTION MISC_CONNECT END-EXEC.

           EXEC SQL
               FETCH LNSSP_CURSOR INTO 
                 :WS-ID,
                 :WS-BRANCH,
                 :WS-ACCTNO
           END-EXEC.

           IF SQLCODE NOT = 0 
            IF SQLCODE NOT = 100
             PERFORM SQL-ERROR.

           EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
       UPDATE-LN-CSR-ROW.
           MOVE "MBSQL CSR"     TO E-FILE.
           MOVE SPACES          TO E-KEYX.
           MOVE "UPDATE ERROR"  TO E-MSG.
           MOVE "UT/MBSQLT"     TO E-PROG.

      *       WHERE CURRENT OF LNA_CURSOR
      *         AND LN_ACCTNO = :WS-ACCTNO

           EXEC SQL
            UPDATE DBO.LNFILE
             SET LN_UNWIND_REASON_CODE  = '3'
             WHERE LN_OWNBR = :WS-BRANCH
               AND LN_ACCTNO = :WS-ACCTNO
           END-EXEC.

           IF SQLCODE NOT = 0
             PERFORM SQL-ERROR.

      *-----------------------------------------------------------------
       UPDATE-LN-CSR-ROW-SSP.
           MOVE "MBSQL CSR"     TO E-FILE.
           MOVE SPACES          TO E-KEYX.
           MOVE "UPDATE ERROR"  TO E-MSG.
           MOVE "UT/MBSQLT"     TO E-PROG.

           MOVE '2' TO SQL-UPDATE-CONN-USE-SW.    *> CHANGE CONNECTION TO UPDATE_CONNECT
           PERFORM SQL-SWAP-UPDATE-CONNECTION.

           EXEC SQL
            UPDATE DBO.LNFILE
             SET LN_UNWIND_REASON_CODE  = :WS-UPDATE-VAULE
             WHERE LN_OWNBR = :WS-BRANCH
               AND LN_ACCTNO = :WS-ACCTNO
           END-EXEC.

           IF SQLCODE NOT = 0
             PERFORM SQL-ERROR.

           MOVE '1' TO SQL-UPDATE-CONN-USE-SW.    *> CHANGE CONNECTION BACK TO C1
           PERFORM SQL-SWAP-UPDATE-CONNECTION.

      *-----------------------------------------------------------------
       CLOSE-LN-CSR.
           MOVE "CURSOR"         TO E-FILE.
           MOVE SPACES           TO E-KEYX.
           MOVE "CLOSE ERROR"    TO E-MSG.
           MOVE "UT/MBSQLT"      TO E-PROG.

           EXEC SQL
                CLOSE LNA_CURSOR
           END-EXEC.

      *-----------------------------------------------------------------
       CLOSE-LN3-CSR.
           MOVE "CURSOR"         TO E-FILE.
           MOVE SPACES           TO E-KEYX.
           MOVE "CLOSE ERROR"    TO E-MSG.
           MOVE "UT/MBSQLT"      TO E-PROG.

           EXEC SQL SET CONNECTION MISC_CONNECT END-EXEC.
           EXEC SQL
                CLOSE LN3_CURSOR
           END-EXEC.
           EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
       CLOSE-LN-CSR-SSP.
           MOVE "CURSOR"         TO E-FILE.
           MOVE SPACES           TO E-KEYX.
           MOVE "CLOSE ERROR"    TO E-MSG.
           MOVE "UT/MBSQLT"      TO E-PROG.

          EXEC SQL SET CONNECTION MISC_CONNECT END-EXEC.

           EXEC SQL
                CLOSE LNSSP_CURSOR
           END-EXEC.

          EXEC SQL SET CONNECTION C1 END-EXEC.

      * SECTIONS ***
       COPY "LIBLP/ARRAYSP.CPY". 
       COPY "LIBGB/AGEING.CPY".
       COPY "LIBGB/DATER.CPY".
       COPY "LIBLP/LPCEPP.CPY".
       COPY "LIBGB/SPPDTH.CPY".
       COPY "LIBLP/LPPDUE.CPY".
       COPY "LIBLP/LPMDTE.CPY".
       COPY "LIBGB/TIMEIO.CPY".
       COPY "LIBLP/LPEARN.CPY".

      * PARAGRAPHS ***
       IO-ROUTINE SECTION.
       COPY "LIBLP/LPBWGS_SQL.CPY".
       COPY "LIBLP/LPLNGS_SQL.CPY".
       COPY "LIBLP/LPLCGS_SQL.CPY".
       COPY "LIBLP/LPCDGS_SQL.CPY".
       COPY "LIBLP/LPLTP1RN.CPY".
       COPY "LIBLP/LPSP1RN.CPY".
       COPY "LIBLP/LPSPGS_SQL.CPY".
       COPY "LIBLP/LPLTPGS_SQL.CPY".
       COPY "LIBGB/GETENV.CPY".
       COPY "LIBGB/FERRORS.CPY".

      ******************************************************************
      *
      *    E X I T - P R O G R A M
      *
      ******************************************************************
       END-PROGRAM SECTION.

       EXIT-PROG.
           MOVE "GOOD BYE" TO E-MSG.
      D    STOP E-MSG.
           EXIT PROGRAM.
