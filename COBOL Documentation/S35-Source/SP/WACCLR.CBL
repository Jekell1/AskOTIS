       IDENTIFICATION DIVISION.
       PROGRAM-ID.      WACCLR.
      ******************************************************************
      *                 AFTER ACQUISITION CLEAR FILES
      *
      *  DESCRIPTION:   PROGRAM TO CLEAR FILES FOR THE ACQUISITION BRANCH
      *                 SO THEY DONT APPEAR IN THE DATA WAREHOUSE OR
      *                 GET UPDATED TO GL
      *
      * CLEARS TR, BW, LP, LN, LM, NO, LTC, LSM AND IX
      *
      *   FORCES BRANCH ENTRY OF 8000 - 8099 OR 9961
      *
      * BAH 20240906 S35Q-124 TESTING ACQUISITIONS
      * BAH 2025.0313 REMOVED CRNOR2W S35Q-176
      ******************************************************************
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
           "@(#)SP/WACCLR S35 09/06/24-10:08:39 >".

       COPY "LIBGB/GB01GB.CPY".
       COPY "LIBGB/GB01GB_SQL.CPY".
       COPY "LIBGB/GBWSGB.CPY".
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBLP/LP01LN.CPY".
       COPY "LIBLP/LP01LN_SQL.CPY".
       COPY "LIBLP/LPWSLN.CPY".
       COPY "LIBLP/LP01LM.CPY".
       COPY "LIBLP/LP01LM_SQL.CPY".
       COPY "LIBLP/LPWSLM.CPY".
       COPY "LIBLP/LP01NO.CPY".
       COPY "LIBLP/LP01NO_SQL.CPY".
       COPY "LIBLP/LPWSNO.CPY".
       COPY "LIBLP/LP01LC.CPY".
       COPY "LIBLP/LP01LC_SQL.CPY".
       COPY "LIBLP/LPWSLC.CPY".
       COPY "LIBLP/LP01LTC.CPY".
       COPY "LIBLP/LP01LTC_SQL.CPY".
       COPY "LIBLP/LPWSLTC.CPY".
       COPY "LIBLP/LP01LTI.CPY".
       COPY "LIBLP/LP01LTI_SQL.CPY".
       COPY "LIBLP/LPWSLTI.CPY".
       COPY "LIBLP/LP01LTP.CPY".
       COPY "LIBLP/LP01LTP_SQL.CPY".
       COPY "LIBLP/LPWSLTP.CPY".
       COPY "LIBLP/LP01LSM.CPY".
       COPY "LIBLP/LP01LSM_SQL.CPY".
       COPY "LIBLP/LPWSLSM.CPY".
       COPY "LIBLP/LP01IX.CPY".
       COPY "LIBLP/LP01IX_SQL.CPY".
       COPY "LIBLP/LPWSIX.CPY".
       COPY "LIBLP/LP01BW.CPY".
       COPY "LIBLP/LP01BW_SQL.CPY".
       COPY "LIBLP/LPWSBW.CPY".
       COPY "LIBLP/LP01LP.CPY".
       COPY "LIBLP/LP01LP_SQL.CPY".
       COPY "LIBLP/LPWSLP.CPY".
       COPY "LIBLP/LP01TRB.CPY".
       COPY "LIBLP/LP01TRB_SQL.CPY".
       COPY "LIBLP/LPWSTRB.CPY".
       COPY "LIBLP/LP01TRC.CPY".
       COPY "LIBLP/LP01TRC_SQL.CPY".
       COPY "LIBLP/LPWSTRC.CPY".
       COPY "LIBLP/LP01TRD.CPY".
       COPY "LIBLP/LP01TRD_SQL.CPY".
       COPY "LIBLP/LPWSTRD.CPY".
       COPY "LIBLP/LP01TRI.CPY".
       COPY "LIBLP/LP01TRI_SQL.CPY".
       COPY "LIBLP/LPWSTRI.CPY".
       COPY "LIBLP/LP01TRJ.CPY".
       COPY "LIBLP/LP01TRJ_SQL.CPY".
       COPY "LIBLP/LPWSTRJ.CPY".
       COPY "LIBLP/LP01TRK.CPY".
       COPY "LIBLP/LP01TRK_SQL.CPY".
       COPY "LIBLP/LPWSTRK.CPY".
       COPY "LIBLP/LP01TRL.CPY".
       COPY "LIBLP/LP01TRL_SQL.CPY".
       COPY "LIBLP/LPWSTRL.CPY".
       COPY "LIBLP/LP01TRP.CPY".
       COPY "LIBLP/LP01TRP_SQL.CPY".
       COPY "LIBLP/LPWSTRP.CPY".
       COPY "LIBLP/LP01TRS.CPY".
       COPY "LIBLP/LP01TRS_SQL.CPY".
       COPY "LIBLP/LPWSTRS.CPY".
       COPY "LIBLP/LP01TRW.CPY".
       COPY "LIBLP/LP01TRW_SQL.CPY".
       COPY "LIBLP/LPWSTRW.CPY".
       COPY "LIBLP/LP01TRX.CPY".
       COPY "LIBLP/LP01TRX_SQL.CPY".
       COPY "LIBLP/LPWSTRX.CPY".

       01  LN-WORKERS.
           03  LN-FEED          PIC 99     VALUE 0.
           03  LN-COUNT         PIC 999    VALUE 99.
           03  LN-WK            PIC 999    VALUE 0.
       01  PAGE-NUMBER          PIC 9(4)   VALUE 0.

       01  WK-STATUS            PIC X     VALUE SPACES.
 
       01  ERRCD            PIC X.
       01  BEG-BRANCH       PIC 9(4) VALUE 0.
       01  ACCEPT-BUF       PIC X    VALUE SPACES.
       01  HOLD-SYSDATE     PIC 9(8).
       01  HOLD-SYS        REDEFINES HOLD-SYSDATE.
           03  HOLD-SYS-CC PIC 99.
           03  HOLD-SYS-YR PIC 99.
           03  HOLD-SYS-MO PIC 99.
           03  HOLD-SYS-DA PIC 99.


       01  LOAN-DISPLAY-NAME    PIC X(4) VALUE "MES.".
       01  LOAN-DISPLAY.
           03  FILLER           PIC X(21)  VALUE
               "PROCESSING.....:".
           03  LOAN-NAME        PIC X(15) VALUE " ".
           03  LOAN-COUNTER     PIC Z(9).
           03  FILLER           PIC X VALUE " ".

      *----------------------------------------------------------------
       01  HEAD1.
           03  H-DATE               PIC X(8) VALUE SPACES.
           03  FILLER               PIC XX   VALUE SPACES.
           03  H-HR                 PIC 99   VALUE 0.
           03  FILLER               PIC X    VALUE ":".
           03  H-MIN                PIC 99   VALUE 0.
           03  FILLER               PIC X(10) VALUE SPACES.
           03  H-NAME               PIC X(40).
           03  FILLER               PIC X(10).
           03  FILLER               PIC X(5)  VALUE "PAGE ".
           03  H-PAGE-NO            PIC ZZZ9.
       01  HEAD2.
           03  FILLER               PIC X(10) VALUE "USER ID:".
           03  H-USERID             PIC X(10).
           03  FILLER               PIC X(10) VALUE SPACES.
           03  FILLER               PIC X(40) VALUE
               "WORLD AUTOMATED CONVERSION LISTING".
       01  HEAD3.
           03  FILLER               PIC X(15) VALUE "ACCTNO".
           03  FILLER               PIC X(13) VALUE "SSNO".
           03  FILLER               PIC X(18) VALUE "NAME".
           03  FILLER               PIC X(15) VALUE "PRINCIPAL".
           03  FILLER               PIC X(12) VALUE "MESSAGE".
 
       01  DETAIL-LINE              PIC X(80).
       01  DETAIL-LINE1 REDEFINES DETAIL-LINE.
           03  D-BRNO               PIC Z(4).
           03  FILLER               PIC X(2).
           03  D-ACCTNO             PIC Z(6).
           03  FILLER               PIC X(2).
           03  D-SSNO               PIC 999/99/9999.
           03  FILLER               PIC XX.
           03  D-NAME               PIC X(15).
           03  FILLER               PIC X(2).
           03  D-PRINBAL            PIC Z(7).99.
           03  FILLER               PIC X(2).
           03  D-MESSAGE            PIC X(25).

       01  DETAIL-LINE2 REDEFINES DETAIL-LINE.
           03  R-COUNTERX    PIC X(20).
           03  R-COUNTER     PIC Z(8)9.
           03  FILLER        PIC X(2).
           03  R-BALANCES    PIC Z(9).99-.
           03  R-ALPHA REDEFINES R-BALANCES PIC X(13).
 
       COPY "LIBGB/SPPDTHW.CPY".
       COPY "LIBGB/GPENVW.CPY".
       COPY "LIBLP/LPCEPPW.CPY".
       COPY "LIBLP/LPMDTEW.CPY".
       COPY "LIBLP/LPAPRZW.CPY".
       COPY "LIBLP/LPAPRSW.CPY".
       COPY "LIBLP/LPAMTSW.CPY".
       COPY "LIBLP/LPUPERW.CPY".
       COPY "LIBLP/LPUPWKW.CPY".
       COPY "LIBLP/SETCTBLW.CPY".
       COPY "LIBLP/LPPDUEW.CPY".
       COPY "LIBLP/LPAERNW.CPY".
       COPY "LIBLP/LPUERNW.CPY".
       COPY "LIBLP/LPCERNW.CPY".
       COPY "LIBLP/LPITRMW.CPY".
       COPY "LIBGB/RBACTW.CPY".
       COPY "LIBLP/LPEADJW.CPY".
       COPY "LIBLP/LPREGZW.CPY".
       COPY "LIBLP/LPPOFFW.CPY".
       COPY "LIBGB/AGEINGW.CPY".
       COPY "LIBGB/REBATEW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBCL/CREAIXW.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBLP/LPXDTEW.CPY".
       COPY "LIBGB/GETPRW.CPY".
       01  WACCLR-WINDOW-HANDLE PIC X(10).
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBSP/WACCLR_DEF.CPY".
       COPY "LIBSP/WACCLR_WKS.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      /***************************************
      *     P R O G R A M   C O N T R O L    *
      ****************************************
       COPY "LIBGB/GETFMW.CPY".

       SCREEN SECTION.
       COPY "LIBSP/WACCLR_SCN.CPY".

       PROCEDURE DIVISION
             USING FORM-PATHNAME EXIT-PATHNAME.

       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
                PR-FILE.
       COPY "LIBGB/DECLARE.CPY".
           PERFORM CLOSE-TRX1-FILE.
           PERFORM CLOSE-TRW1-FILE.
           PERFORM CLOSE-TRS1-FILE.
           PERFORM CLOSE-TRP1-FILE.
           PERFORM CLOSE-TRL1-FILE.
           PERFORM CLOSE-TRK1-FILE.
           PERFORM CLOSE-TRJ1-FILE.
           PERFORM CLOSE-TRI1-FILE.
           PERFORM CLOSE-TRD1-FILE.
           PERFORM CLOSE-TRC1-FILE.
           PERFORM CLOSE-TRB1-FILE.
           PERFORM CLOSE-LTP1-FILE.
           PERFORM CLOSE-LTI1-FILE.
           PERFORM CLOSE-LTC1-FILE.
           PERFORM CLOSE-LSM1-FILE.
           PERFORM CLOSE-LP1-FILE.
           PERFORM CLOSE-NO1-FILE.
           PERFORM CLOSE-LM1-FILE.
           PERFORM CLOSE-IX1-FILE.
           PERFORM CLOSE-BW1-FILE.
           PERFORM CLOSE-LN1-FILE.
           PERFORM CLOSE-BR-FILE.
           CLOSE PR-FILE.
       COPY "LIBGB/DECLAR2.CPY".
 
      *****************************
       MAIN-MODULE SECTION.
           PERFORM SQL-CONNECT.
      *****************************
       COPY "LIBGB/CHKSEC.CPY".
       MAIN-ROUTINE.
      D    STOP MESS.
           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "WACCLR" TO VDU-FORM-NAME.

           DISPLAY FLOATING GRAPHICAL WINDOW
                   SCREEN LINE 01 * VDU-WIN-L-OFFSET
                   SCREEN COL 01 * VDU-WIN-C-OFFSET
                   CELL SIZE IS LABEL FONT
                   SIZE 80 + VDU-WIN-W-MOD
                   LINES 24 + VDU-WIN-H-MOD
                   BLANK SCREEN
                   HANDLE IS WACCLR-WINDOW-HANDLE.

           PERFORM FORM-RESET.

           PERFORM INITIALIZATION.
           IF ERRCD NOT = " "
              GO TO END-PROGRAM.

      *  SCREEN DISPLAY
           MOVE LOAN-DISPLAY      TO WR-BUF.
           MOVE LOAN-DISPLAY-NAME TO WR-LIST.
           PERFORM CALL-WRFORM.

           PERFORM PURGE-OPEN-OUTPUT-TR.
           PERFORM PURGE-OPEN-OUTPUT-BW.
           PERFORM PURGE-OPEN-OUTPUT-LP.
           PERFORM PURGE-OPEN-OUTPUT-LN.
           PERFORM PURGE-OPEN-OUTPUT-LM.
           PERFORM PURGE-OPEN-OUTPUT-NO.
           PERFORM PURGE-OPEN-OUTPUT-LTC.
           PERFORM PURGE-OPEN-OUTPUT-LSM.
           PERFORM PURGE-OPEN-OUTPUT-IX.

       END-ROUTINE.

           GO TO END-PROGRAM.

      ***********************************
       INITIALIZATION SECTION.
      ***********************************
           MOVE " " TO ERRCD.

           PERFORM OPEN-BR-FILE.

       GETPR.
           MOVE WACCLR-QUEUE-POS TO Q-POS.
           MOVE WACCLR-COPIES-POS TO C-POS.
           MOVE WACCLR-PRU-POS TO P-POS.
           MOVE SPACES TO Q-BUF C-BUF P-BUF.
           PERFORM PRU-IN.
           IF STAT NOT = "00"
              GO TO ENDIT.


       ENTER-BEG-BRANCH.
           MOVE "ENNN040BEG-BRANCH." TO VDU.
           PERFORM SCREENIO.
           IF (        F1-KEY ) GO TO GETPR.
           IF (        F7-KEY ) GO TO ENDIT.
           IF NOT ( ENTER-KEY ) GO TO ENTER-BEG-BRANCH.
           MOVE VDUNUM0 TO BEG-BRANCH.
           IF BEG-BRANCH < 8000 OR > 8099
              IF BEG-BRANCH NOT = 9961
                 MOVE "INVALID BRANCH" TO MESS PERFORM SEND-MESS
                 GO TO ENTER-BEG-BRANCH.

           MOVE BEG-BRANCH TO BR-NO.
           PERFORM READ-BR-FILE.
           IF IO-FG NOT = 0
              MOVE "INVALID BRANCH" TO MESS PERFORM SEND-MESS
              GO TO ENTER-BEG-BRANCH.

           PERFORM SET-FILE-PATHS.

       OK-YN.
           MOVE "E  A010SEL." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO GETPR.
           IF VDUSTATUS = "1>" GO TO ENDIT.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO OK-YN.
           MOVE VDUBUF TO ACCEPT-BUF.

           IF ACCEPT-BUF = "N" GO TO ENDIT.
           IF ACCEPT-BUF NOT = "Y" GO TO OK-YN.

           MOVE EXT-CONAME-SYSDATE TO ALP-DATE.
           PERFORM CONVERT-ALPDATE-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD TO HOLD-SYSDATE.

           MOVE SPACES             TO DETAIL-LINE.
           MOVE EXT-CONAME-CONAME  TO H-NAME.
           MOVE EXT-CONAME-SYSDATE TO H-DATE.
           MOVE EXT-CONAME-USER-ID TO H-USERID.

           PERFORM OPEN-PR-FILE.

           GO TO EXIT-INITIALIZE.
       ENDIT.
           MOVE "X" TO ERRCD.
       EXIT-INITIALIZE.
           EXIT.

      ***********************************************
       PURGE-OPEN-OUTPUT-IX SECTION.
      ***********************************************

           PERFORM OPEN-IX1-FILE.

           MOVE IX-PATH-OWNBR TO IX-BRNO.
           MOVE SPACES        TO IX-CODE.
           MOVE ZEROES        TO IX-ACCTNO.
           MOVE ZEROES        TO IX-SEQNO.

           PERFORM START-IX1-FILE.

       PURGE-OPEN-OUTPUT-IX-NEXT.
           PERFORM READ-IX1-FILE-NEXT.
           IF ( IO-BAD ) OR ( IX-BRNO NOT = IX-PATH-OWNBR )
              PERFORM CLOSE-IX1-FILE
              GO TO PURGE-OPEN-OUTPUT-IX-EXIT.

           PERFORM DELETE-IX1-FILE.
           IF ( IO-BAD )
              GO TO IO-ERROR.

           GO TO PURGE-OPEN-OUTPUT-IX-NEXT.
       PURGE-OPEN-OUTPUT-IX-EXIT.
           EXIT.

      ***********************************************
       PURGE-OPEN-OUTPUT-TR SECTION.
      ***********************************************
           PERFORM REMOVE-TRB-RECORDS.
           PERFORM REMOVE-TRC-RECORDS.
           PERFORM REMOVE-TRD-RECORDS.
           PERFORM REMOVE-TRI-RECORDS.
           PERFORM REMOVE-TRJ-RECORDS.
           PERFORM REMOVE-TRK-RECORDS.
           PERFORM REMOVE-TRL-RECORDS.
           PERFORM REMOVE-TRP-RECORDS.
           PERFORM REMOVE-TRS-RECORDS.
           PERFORM REMOVE-TRW-RECORDS.
           PERFORM REMOVE-TRX-RECORDS.

       PURGE-OPEN-OUTPUT-TR-EXIT.
           EXIT.

      *****************************************
       REMOVE-TRB-RECORDS SECTION.
      *****************************************
           PERFORM OPEN-TRB1-FILE.

           MOVE BR-NO    TO TB-BRNO.
      * CAN NOT USE 0 IN DATABASE
           MOVE 19000101 TO TB-DATE.
           MOVE 0        TO TB-CLASS TB-ACCTNO
                            TB-LAWCODE TB-LINE.

           PERFORM START-TRB1-FILE.
       NEXT-TRB1-DELETE.
           PERFORM READ-TRB1-FILE-NEXT.
           IF (IO-BAD) OR (TB-BRNO NOT = TRB-PATH-OWNBR)
              GO TO REMOVE-TRB-RECORDS-EXIT.

           PERFORM DELETE-TRB1-FILE.

           GO TO NEXT-TRB1-DELETE.

       REMOVE-TRB-RECORDS-EXIT.
           PERFORM CLOSE-TRB1-FILE.

      *****************************************
       REMOVE-TRC-RECORDS SECTION.
      *****************************************
           PERFORM OPEN-TRC1-FILE.

           MOVE BR-NO    TO TC-BRNO.
           MOVE 19000101 TO TC-KEY-DATE.
           MOVE 0        TO TC-CKNO TC-CLASS TC-SEQ.

           PERFORM START-TRC1-FILE.
       NEXT-TRC1-DELETE.
           PERFORM READ-TRC1-FILE-NEXT.
           IF (IO-BAD) OR (TC-BRNO NOT = TRC-PATH-OWNBR)
              GO TO REMOVE-TRC-RECORDS-EXIT.

           PERFORM DELETE-TRC1-FILE.

           GO TO NEXT-TRC1-DELETE.

       REMOVE-TRC-RECORDS-EXIT.
           PERFORM CLOSE-TRC1-FILE.

      *****************************************
       REMOVE-TRD-RECORDS SECTION.
      *****************************************
           PERFORM OPEN-TRD1-FILE.

           MOVE BR-NO    TO TD-BRNO.
      * CAN NOT USE 0 IN DATABASE
           MOVE 19000101 TO TD-DATE.

           PERFORM START-TRD1-FILE.
       NEXT-TRD1-DELETE.
           PERFORM READ-TRD1-FILE-NEXT.
           IF (IO-BAD) OR (TD-BRNO NOT = TRD-PATH-OWNBR)
              GO TO REMOVE-TRD-RECORDS-EXIT.

           PERFORM DELETE-TRD1-FILE.

           GO TO NEXT-TRD1-DELETE.

       REMOVE-TRD-RECORDS-EXIT.
           PERFORM CLOSE-TRD1-FILE.

      *****************************************
       REMOVE-TRI-RECORDS SECTION.
      *****************************************
           PERFORM OPEN-TRI1-FILE.

           MOVE BR-NO    TO TI-BRNO.
           MOVE 19000101 TO TI-KEY-DATE.
           MOVE 0        TO TI-SEQ.

           PERFORM START-TRI1-FILE.
       NEXT-TRI1-DELETE.
           PERFORM READ-TRI1-FILE-NEXT.
           IF (IO-BAD) OR (TI-BRNO NOT = TRI-PATH-OWNBR)
              GO TO REMOVE-TRI-RECORDS-EXIT.

           PERFORM DELETE-TRI1-FILE.

           GO TO NEXT-TRI1-DELETE.

       REMOVE-TRI-RECORDS-EXIT.
           PERFORM CLOSE-TRI1-FILE.

      *****************************************
       REMOVE-TRJ-RECORDS SECTION.
      *****************************************
           PERFORM OPEN-TRJ1-FILE.

           MOVE BR-NO    TO TJ-BRNO.
      * CAN NOT USE 0 IN DATABASE
           MOVE 19000101 TO TJ-DATE.
           MOVE 0        TO TJ-CLASS TJ-ACCTNO
                            TJ-LAWCODE TJ-LINE.

           PERFORM START-TRJ1-FILE.
       NEXT-TRJ1-DELETE.
           PERFORM READ-TRJ1-FILE-NEXT.
           IF (IO-BAD) OR (TJ-BRNO NOT = TRJ-PATH-OWNBR)
              GO TO REMOVE-TRJ-RECORDS-EXIT.

           PERFORM DELETE-TRJ1-FILE.

           GO TO NEXT-TRJ1-DELETE.

       REMOVE-TRJ-RECORDS-EXIT.
           PERFORM CLOSE-TRJ1-FILE.

      *****************************************
       REMOVE-TRK-RECORDS SECTION.
      *****************************************
           PERFORM OPEN-TRK1-FILE.

           MOVE BR-NO    TO TK-BRNO.
      * CAN NOT USE 0 IN DATABASE
           MOVE 19000101 TO TK-DATE.
           MOVE 0        TO TK-CLASS TK-ACCTNO
                            TK-LAWCODE TK-LINE.

           PERFORM START-TRK1-FILE.
       NEXT-TRK1-DELETE.
           PERFORM READ-TRK1-FILE-NEXT.
           IF (IO-BAD) OR (TK-BRNO NOT = TRK-PATH-OWNBR)
              GO TO REMOVE-TRK-RECORDS-EXIT.

           PERFORM DELETE-TRK1-FILE.

           GO TO NEXT-TRK1-DELETE.

       REMOVE-TRK-RECORDS-EXIT.
           PERFORM CLOSE-TRK1-FILE.

      *****************************************
       REMOVE-TRL-RECORDS SECTION.
      *****************************************
           PERFORM OPEN-TRL1-FILE.

           MOVE BR-NO    TO TL-BRNO.
      * CAN NOT USE 0 IN DATABASE
           MOVE 19000101 TO TL-DATE.
           MOVE 0        TO TL-CLASS TL-ACCTNO
                            TL-LAWCODE TL-LINE.

           PERFORM START-TRL1-FILE.
       NEXT-TRL1-DELETE.
           PERFORM READ-TRL1-FILE-NEXT.
           IF (IO-BAD) OR (TL-BRNO NOT = TRL-PATH-OWNBR)
              GO TO REMOVE-TRL-RECORDS-EXIT.

           PERFORM DELETE-TRL1-FILE.

           GO TO NEXT-TRL1-DELETE.

       REMOVE-TRL-RECORDS-EXIT.
           PERFORM CLOSE-TRL1-FILE.

      *****************************************
       REMOVE-TRP-RECORDS SECTION.
      *****************************************
           PERFORM OPEN-TRP1-FILE.

           MOVE BR-NO    TO TP-BRNO.
      *    MOVE " "      TO TP-CODE.
      * CAN NOT USE 0 IN DATABASE
           MOVE 19000101 TO TP-DATE.
           MOVE 0        TO TP-POOLID TP-CLASS TP-ACCTNO TP-LINE.

           PERFORM START-TRP1-FILE.
       NEXT-TRP1-DELETE.
           PERFORM READ-TRP1-FILE-NEXT.
           IF (IO-BAD) OR (TP-BRNO NOT = TRP-PATH-OWNBR)
              GO TO REMOVE-TRP-RECORDS-EXIT.

           PERFORM DELETE-TRP1-FILE.

           GO TO NEXT-TRP1-DELETE.

       REMOVE-TRP-RECORDS-EXIT.
           PERFORM CLOSE-TRP1-FILE.

      *****************************************
       REMOVE-TRS-RECORDS SECTION.
      *****************************************
           PERFORM OPEN-TRS1-FILE.

           MOVE BR-NO    TO TS-BRNO.
      * CAN NOT USE 0 IN DATABASE
           MOVE 19000101 TO TS-DATE.
           MOVE 0        TO TS-CLASS TS-ACCTNO TS-LAWCODE TS-LINE.

           PERFORM START-TRS1-FILE.
       NEXT-TRS1-DELETE.
           PERFORM READ-TRS1-FILE-NEXT.
           IF (IO-BAD) OR (TS-BRNO NOT = TRS-PATH-OWNBR)
              GO TO REMOVE-TRS-RECORDS-EXIT.

           PERFORM DELETE-TRS1-FILE.

           GO TO NEXT-TRS1-DELETE.

       REMOVE-TRS-RECORDS-EXIT.
           PERFORM CLOSE-TRS1-FILE.

      *****************************************
       REMOVE-TRW-RECORDS SECTION.
      *****************************************
           PERFORM OPEN-TRW1-FILE.

           MOVE BR-NO    TO TW-BRNO.
      * CAN NOT USE 0 IN DATABASE
           MOVE 19000101 TO TW-DATE.
           MOVE 0        TO TW-TILLNO.

           PERFORM START-TRW1-FILE.
       NEXT-TRW1-DELETE.
           PERFORM READ-TRW1-FILE-NEXT.
           IF (IO-BAD) OR (TW-BRNO NOT = TRW-PATH-OWNBR)
              GO TO REMOVE-TRW-RECORDS-EXIT.

           PERFORM DELETE-TRW1-FILE.

           GO TO NEXT-TRW1-DELETE.

       REMOVE-TRW-RECORDS-EXIT.
           PERFORM CLOSE-TRW1-FILE.

      *****************************************
       REMOVE-TRX-RECORDS SECTION.
      *****************************************
           PERFORM OPEN-TRX1-FILE.

           MOVE BR-NO    TO TX-BRNO.
           MOVE 19000101 TO TX-DATE.
           MOVE 0        TO TX-CLASS TX-ACCTNO
                            TX-LAWCODE TX-LINE.

           PERFORM START-TRX1-FILE.
       NEXT-TRX1-DELETE.
           PERFORM READ-TRX1-FILE-NEXT.
           IF (IO-BAD) OR (TX-BRNO NOT = TRX-PATH-OWNBR)
              GO TO REMOVE-TRX-RECORDS-EXIT.

           PERFORM DELETE-TRX1-FILE.

           GO TO NEXT-TRX1-DELETE.

       REMOVE-TRX-RECORDS-EXIT.
           PERFORM CLOSE-TRX1-FILE.

      ***********************************************
       PURGE-OPEN-OUTPUT-BW SECTION.
      ***********************************************

           PERFORM OPEN-BW1-FILE.

           MOVE BR-NO         TO BW-OWNBR.
           MOVE 0             TO BW-SSNO.

           PERFORM START-BW1-FILE.

       PURGE-OPEN-OUTPUT-BW-NEXT.
           PERFORM READ-BW1-FILE-NEXT.
           IF ( IO-BAD ) OR ( BW-OWNBR NOT = BW-PATH-OWNBR )
              PERFORM CLOSE-BW1-FILE
              GO TO PURGE-OPEN-OUTPUT-BW-EXIT.

           PERFORM DELETE-BW1-FILE.
           IF ( IO-BAD )
              GO TO IO-ERROR.

           GO TO PURGE-OPEN-OUTPUT-BW-NEXT.


       PURGE-OPEN-OUTPUT-BW-EXIT.
           EXIT.

      ***********************************************
       PURGE-OPEN-OUTPUT-LN SECTION.
      ***********************************************

           PERFORM OPEN-LN1-FILE.
           MOVE LN-PATH-OWNBR TO LN-OWNBR.
           MOVE 0             TO LN-ACCTNO.

           PERFORM START-LN1-FILE.

       PURGE-OPEN-OUTPUT-LN-NEXT.
           PERFORM READ-LN1-FILE-NEXT.
           IF ( IO-BAD ) OR ( LN-OWNBR NOT = LN-PATH-OWNBR )
              PERFORM CLOSE-LN1-FILE
              GO TO PURGE-OPEN-OUTPUT-LN-EXIT.

           PERFORM DELETE-LN1-FILE.
           IF ( IO-BAD )
              GO TO IO-ERROR.

           GO TO PURGE-OPEN-OUTPUT-LN-NEXT.

       PURGE-OPEN-OUTPUT-LN-EXIT.
           EXIT.

      ***********************************************
       PURGE-OPEN-OUTPUT-LP SECTION.
      ***********************************************

           PERFORM OPEN-LP1-FILE.

           MOVE LP-PATH-OWNBR TO LP-BRNO.
           MOVE 0             TO LP-ACCTNO.
           MOVE 0             TO LP-SEQNO.

           PERFORM START-LP1-FILE.

       PURGE-OPEN-OUTPUT-LP-NEXT.
           PERFORM READ-LP1-FILE-NEXT.
           IF ( IO-BAD ) OR ( LP-BRNO NOT = LP-PATH-OWNBR )
              PERFORM CLOSE-LP1-FILE
              GO TO PURGE-OPEN-OUTPUT-LP-EXIT.

           PERFORM DELETE-LP1-FILE.
           IF ( IO-BAD )
              GO TO IO-ERROR.

           GO TO PURGE-OPEN-OUTPUT-LP-NEXT.
       PURGE-OPEN-OUTPUT-LP-EXIT.
           EXIT.

      ***********************************************
       PURGE-OPEN-OUTPUT-NO SECTION.
      ***********************************************

           PERFORM OPEN-NO1-FILE.

           MOVE BR-NO         TO NO-BRNO.
           MOVE 0             TO NO-ACCTNO.

           PERFORM START-NO1-FILE.

       PURGE-OPEN-OUTPUT-NO-NEXT.
           PERFORM READ-NO1-FILE-NEXT.
           IF ( IO-BAD ) OR ( NO-BRNO NOT = NO-PATH-OWNBR )
              PERFORM CLOSE-NO1-FILE
              GO TO PURGE-OPEN-OUTPUT-NO-EXIT.

           PERFORM DELETE-NO1-FILE.
           IF ( IO-BAD )
              GO TO IO-ERROR.

           GO TO PURGE-OPEN-OUTPUT-NO-NEXT.

       PURGE-OPEN-OUTPUT-NO-EXIT.
           EXIT.

      ***********************************************
       PURGE-OPEN-OUTPUT-LM SECTION.
      ***********************************************

           PERFORM OPEN-LM1-FILE.
           MOVE LM-PATH-OWNBR TO LM-BRNO.
           MOVE 0             TO LM-ACCTNO LM-DATE-REVERSED LM-SEQ.

           PERFORM START-LM1-FILE.

       PURGE-OPEN-OUTPUT-LM-NEXT.
           PERFORM READ-LM1-FILE-NEXT.
           IF ( IO-BAD ) OR ( LM-BRNO NOT = LM-PATH-OWNBR )
              PERFORM CLOSE-LM1-FILE
              GO TO PURGE-OPEN-OUTPUT-LM-EXIT.

           PERFORM DELETE-LM1-FILE.

           IF ( IO-BAD )
              GO TO IO-ERROR.

           GO TO PURGE-OPEN-OUTPUT-LM-NEXT.

       PURGE-OPEN-OUTPUT-LM-EXIT.
           EXIT.

      ***********************************************
       PURGE-OPEN-OUTPUT-LTC SECTION.
      ***********************************************

           PERFORM OPEN-LTC1-FILE.
           MOVE LTC-PATH-OWNBR TO LTC-BRNO.
           MOVE ZEROES         TO LTC-ACCTNO.
           MOVE ZEROES         TO LTC-SEQNO.

           PERFORM START-LTC1-FILE.

       PURGE-OPEN-OUTPUT-LTC-NEXT.
           PERFORM READ-LTC1-FILE-NEXT.
           IF ( IO-BAD ) OR ( LTC-BRNO NOT = LTC-PATH-OWNBR )
              PERFORM CLOSE-LTC1-FILE
              GO TO PURGE-OPEN-OUTPUT-LTC-EXIT.

           PERFORM DELETE-LTC1-FILE.
           IF ( IO-BAD )
              GO TO IO-ERROR.

           GO TO PURGE-OPEN-OUTPUT-LTC-NEXT.

       PURGE-OPEN-OUTPUT-LTC-EXIT.
           EXIT.


      ***********************************************
       PURGE-OPEN-OUTPUT-LSM SECTION.
      ***********************************************

           PERFORM OPEN-LSM1-FILE.
           MOVE LSM-PATH-OWNBR TO LSM-BRNO.
           MOVE ZEROES         TO LSM-ACCTNO.
           MOVE ZEROES         TO LSM-SEQNO.

           PERFORM START-LSM1-FILE.

       PURGE-OPEN-OUTPUT-LSM-NEXT.
           PERFORM READ-LSM1-FILE-NEXT.
           IF ( IO-BAD ) OR ( LSM-BRNO NOT = LSM-PATH-OWNBR )
              PERFORM CLOSE-LSM1-FILE
              GO TO PURGE-OPEN-OUTPUT-LSM-EXIT.

           PERFORM DELETE-LSM1-FILE.
           IF ( IO-BAD )
              GO TO IO-ERROR.

           GO TO PURGE-OPEN-OUTPUT-LSM-NEXT.

       PURGE-OPEN-OUTPUT-LSM-EXIT.
           EXIT.

       COPY "LIBGB/GPENV.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/DATER.CPY".

      ****************************************************
       FORM-RESET SECTION.
      ****************************************************
           MOVE EXT-CONAME-CONAME TO WACCLR-CONAME.
           MOVE EXT-CONAME-SYSDATE TO WACCLR-SYSDATE.
           DISPLAY WACCLR-SCREEN UPON WACCLR-WINDOW-HANDLE.
           MOVE WACCLR-FORM-FIELDS TO WR-BUF.
           MOVE WACCLR-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      ****************************************************
       SETUP-LINK-INFO SECTION.
      ****************************************************
           MOVE WACCLR-HELP--FILE TO HELP-LINK-FILE.
           MOVE WACCLR-HELP--DIR  TO HELP-LINK-DIR.

      ****************************************************
       VDU-CONVERT-FIELD SECTION.
      ****************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBSP/WACCLR_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ************************************
       MISC-PARAGRAPH SECTION.
      ************************************
       COPY "LIBGB/GETPR.CPY".
       COPY "LIBGB/ACCESS.CPY".

       SET-FILE-PATHS.
           MOVE EXT-FILPATH-BASE TO FILPATH-BASE.
           MOVE BR-MACHINE       TO FILPATH-MACHINE.
           MOVE BR-DATA-PATH     TO FILPATH-DATA.
           MOVE "B"        TO BW-PATH-OVERRIDE
                              IX-PATH-OVERRIDE
                              LN-PATH-OVERRIDE
                              LP-PATH-OVERRIDE
                              NO-PATH-OVERRIDE
                              LM-PATH-OVERRIDE
                              LSM-PATH-OVERRIDE
                              LTP-PATH-OVERRIDE
                              LTI-PATH-OVERRIDE
                              LTC-PATH-OVERRIDE
                              TRB-PATH-OVERRIDE
                              TRC-PATH-OVERRIDE
                              TRD-PATH-OVERRIDE
                              TRI-PATH-OVERRIDE
                              TRJ-PATH-OVERRIDE
                              TRK-PATH-OVERRIDE
                              TRL-PATH-OVERRIDE
                              TRP-PATH-OVERRIDE
                              TRS-PATH-OVERRIDE
                              TRW-PATH-OVERRIDE
                              TRX-PATH-OVERRIDE.

      *********************************
       IO-ROUTINE SECTION.
      *********************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBLP/LPTRXGS_SQL.CPY".
       COPY "LIBLP/LPTRWGS_SQL.CPY".
       COPY "LIBLP/LPTRSGS_SQL.CPY".
       COPY "LIBLP/LPTRPGS_SQL.CPY".
       COPY "LIBLP/LPTRLGS_SQL.CPY".
       COPY "LIBLP/LPTRKGS_SQL.CPY".
       COPY "LIBLP/LPTRJGS_SQL.CPY".
       COPY "LIBLP/LPTRIGS_SQL.CPY".
       COPY "LIBLP/LPTRDGS_SQL.CPY".
       COPY "LIBLP/LPTRCGS_SQL.CPY".
       COPY "LIBLP/LPTRBGS_SQL.CPY".
       COPY "LIBLP/LPLTPGS_SQL.CPY".
       COPY "LIBLP/LPLTIGS_SQL.CPY".
       COPY "LIBLP/LPLTCGS_SQL.CPY".
       COPY "LIBLP/LPLSMGS_SQL.CPY".
       COPY "LIBLP/LPLPGS_SQL.CPY".
       COPY "LIBLP/LPLNGS_SQL.CPY".
       COPY "LIBLP/LPLMGS_SQL.CPY".
       COPY "LIBLP/LPNOGS_SQL.CPY".
       COPY "LIBLP/LPIXGS_SQL.CPY".
       COPY "LIBLP/LPBWGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBPRINT.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBLP/LPLTC1IN.CPY".
       COPY "LIBLP/LPLTI1IN.CPY".
       COPY "LIBLP/LPLTP1RN.CPY".
       COPY "LIBLP/LPLSM1IN.CPY".
       COPY "LIBLP/LPLN1IN_NOBI.CPY".
       COPY "LIBLP/LPBW1IN.CPY".
       COPY "LIBLP/LPLP1IN.CPY".
       COPY "LIBLP/LPLM1IN.CPY".
       COPY "LIBLP/LPNO1IN.CPY".
       COPY "LIBLP/LPIX1IN.CPY".
       COPY "LIBLP/LPTRB1IN.CPY".
       COPY "LIBLP/LPTRC1IN.CPY".
       COPY "LIBLP/LPTRD1IN.CPY".
       COPY "LIBLP/LPTRI1IN.CPY".
       COPY "LIBLP/LPTRJ1IN.CPY".
       COPY "LIBLP/LPTRK1IN.CPY".
       COPY "LIBLP/LPTRL1IN.CPY".
       COPY "LIBLP/LPTRP1IN.CPY".
       COPY "LIBLP/LPTRS1IN.CPY".
       COPY "LIBLP/LPTRW1IN.CPY".
       COPY "LIBLP/LPTRX1IN.CPY".
       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO ERRCD.
 
      *********************************
       END-PROGRAM SECTION.
      *********************************
       END-RUN.
           PERFORM CLOSE-FILES.
           PERFORM PRU-OUT.
       EXIT-PROG.
           MOVE "SP/EXTRMU" TO FORM-NAM.
           MOVE FORM-PATH TO FORM-PATHNAME.
       STOP-RUN.
           DESTROY WACCLR-WINDOW-HANDLE.
           EXIT PROGRAM.
