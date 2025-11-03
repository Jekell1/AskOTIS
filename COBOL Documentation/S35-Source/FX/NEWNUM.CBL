       IDENTIFICATION DIVISION.
       PROGRAM-ID.   NEWNUM.
       DATE-WRITTEN. 051303.
      ******************************************************************
      *
      *                      (FX)
      *  DESCRIPTION:  LOAN ACCOUNT NUMBER CHANGE AND ALL FILES
      *                   ASSOCIATED
      *
      *  REV:
      *   030513 BAH REGACC HAS A ACCOUNT NUMBER 0 AND NEEDS TO GIVE IT
      *              A NEW NUMBER
      *   BAH 160218 EXPANDED LM-REC ADDED LM-BRNO, PD0003
      *
      * KEC 2016.0502 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         CHANGED MA-APNO TO USE MA-APP-NUMBER FOR MA1-KEY.
      * BAH 2016.0615 CHANGED DT1-KEY
      *
      * KEC 2016.0913 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         SET LR-BRNO USING BR-NO.
      *
      * KEC 2017.0313 {PR#00000} PARADATA <A30> [JAY CISZEWSKI]
      *          FIXED THE SIZES OF THE HOLD KEY FIELDS.
      *
      * KEC 2017.0510 {PR#00000} PARADATA <A30> [JAY CISZEWSKI]
      *          ADDED 'LTR' FILE.
      *
      * KEC 2018-0822 LOAN APP MODULE REMOVAL FROM A30 [LARM30]
      * BAH 181008 GLOBAL CKFILE
      * BAH 181029 GLOBAL DTFILE
      *  CS 181111 GLOBAL LSLFILE
      *  CS 181113 GLOBAL LSJFILE
      *  CS 181113 GLOBAL LSGFILE
      *  CS 181113 GLOBAL LSMFILE
      *  CS 181114 GLOBAL LSPFILE
      *  CS 181115 GLOBAL LSRFILE
      *  CS 181115 GLOBAL LSXFILE
      *  CS 181116 GLOBAL LSDFILE
      *  CS 181119 GLOBAL LSFFILE
      *  BAH 181126 GLOBAL LHFILE
      *  BAH 181127 GLOBAL LMFILE
      *  BAH 181203 GLOBAL LNFILE
      *  BAH 181219 GLOBAL LPFILE
      *   CS 190102 GLOBAL LTAFILE
      *   CS 190102 GLOBAL LTBFILE
      *   CS 190102 GLOBAL LTCFILE
      *   CS 190102 GLOBAL LTFFILE
      *   CS 190102 GLOBAL LTIFILE
      *  BAH 190107 GLOBAL NOFILE
      *  BAH 190114 GLOBAL LXFILE
      *   CS 190114 GLOBAL LTLFILE
      *   CS 190114 GLOBAL LTPFILE
      *   CS 190114 GLOBAL CJFILE
      *  BAH 20190703 REMOVED LTR PT0150
      *  BAH 20190709 REMOVED LRFILE PT0151
      *  BAH 190918 REMOVED ACCESS-CALL ON LNFILE
      *  BAH 2022.1007 HARD CODED STARTS
      *  BAH 2024.0201 REMOVED NO-FMNO
      * JKC 2024-0304 TRIMMED TRAILING WHITESPACE ON LINES
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "LIBGB/GBFSPR.CPY".
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.
       COPY "LIBGB/GBFDPR.CPY".


      ******************************************************************
      *
      *    W O R K I N G - S T O R A G E - S E C T I O N
      *
      ******************************************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)FX/NEWNUM.CBL S35 03/04/24-15:54:25 >".

      *=================================================================
      *
      *    F I L E - W O R K E R S
      *
      *=================================================================
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBSP/SP01CJ.CPY".
       COPY "LIBSP/SP01CJ_SQL.CPY".
       COPY "LIBSP/SPWSCJ.CPY".
       COPY "LIBLP/LP01BW.CPY".
       COPY "LIBLP/LP01BW_SQL.CPY".
       COPY "LIBLP/LPWSBW.CPY".
       COPY "LIBLP/LP01CK.CPY".
       COPY "LIBLP/LP01CK_SQL.CPY".
       COPY "LIBLP/LPWSCK.CPY".
       COPY "LIBLP/LP01NO.CPY".
       COPY "LIBLP/LP01NO_SQL.CPY".
       COPY "LIBLP/LPWSNO.CPY".
       COPY "LIBLP/LP01DT.CPY".
       COPY "LIBLP/LP01DT_SQL.CPY".
       COPY "LIBLP/LPWSDT.CPY".
       COPY "LIBLP/LP01LE.CPY".
       COPY "LIBLP/LP01LE_SQL.CPY".
       COPY "LIBLP/LPWSLE.CPY".
       COPY "LIBLP/LP01LH.CPY".
       COPY "LIBLP/LP01LH_SQL.CPY".
       COPY "LIBLP/LPWSLH.CPY".
       COPY "LIBLP/LP01LM.CPY".
       COPY "LIBLP/LP01LM_SQL.CPY".
       COPY "LIBLP/LPWSLM.CPY".
       COPY "LIBLP/LP01LN.CPY".
       COPY "LIBLP/LP01LN_SQL.CPY".
       COPY "LIBLP/LPWSLN.CPY".
       COPY "LIBLP/LP01LP.CPY".
       COPY "LIBLP/LP01LP_SQL.CPY".
       COPY "LIBLP/LPWSLP.CPY".
       COPY "LIBLP/LP01LTA.CPY".
       COPY "LIBLP/LP01LTA_SQL.CPY".
       COPY "LIBLP/LPWSLTA.CPY".
       COPY "LIBLP/LP01LTB.CPY".
       COPY "LIBLP/LP01LTB_SQL.CPY".
       COPY "LIBLP/LPWSLTB.CPY".
       COPY "LIBLP/LP01LTC.CPY".
       COPY "LIBLP/LP01LTC_SQL.CPY".
       COPY "LIBLP/LPWSLTC.CPY".
       COPY "LIBLP/LP01LTF.CPY".
       COPY "LIBLP/LP01LTF_SQL.CPY".
       COPY "LIBLP/LPWSLTF.CPY".
       COPY "LIBLP/LP01LTI.CPY".
       COPY "LIBLP/LP01LTI_SQL.CPY".
       COPY "LIBLP/LPWSLTI.CPY".
       COPY "LIBLP/LP01LTL.CPY".
       COPY "LIBLP/LP01LTL_SQL.CPY".
       COPY "LIBLP/LPWSLTL.CPY".
       COPY "LIBLP/LP01LTP.CPY".
       COPY "LIBLP/LP01LTP_SQL.CPY".
       COPY "LIBLP/LPWSLTP.CPY".
       COPY "LIBLP/LPWSIX.CPY".
       COPY "LIBLP/LP01LX.CPY".
       COPY "LIBLP/LP01LX_SQL.CPY".
       COPY "LIBLP/LPWSLX.CPY".
       COPY "LIBLP/LP01LXG.CPY".
       COPY "LIBLP/LP01LXG_SQL.CPY".
       COPY "LIBLP/LPWSLXG.CPY".
       COPY "LIBLP/LP01LXE.CPY".
       COPY "LIBLP/LP01LXE_SQL.CPY".
       COPY "LIBLP/LPWSLXE.CPY".
       COPY "LIBLP/LP01LSD.CPY".
       COPY "LIBLP/LP01LSD_SQL.CPY".
       COPY "LIBLP/LPWSLSD.CPY".
       COPY "LIBLP/LP01LSF.CPY".
       COPY "LIBLP/LP01LSF_SQL.CPY".
       COPY "LIBLP/LPWSLSF.CPY".
       COPY "LIBLP/LP01LSG.CPY".
       COPY "LIBLP/LP01LSG_SQL.CPY".
       COPY "LIBLP/LPWSLSG.CPY".
       COPY "LIBLP/LP01LSJ.CPY".
       COPY "LIBLP/LP01LSJ_SQL.CPY".
       COPY "LIBLP/LPWSLSJ.CPY".
       COPY "LIBLP/LP01LSL.CPY".
       COPY "LIBLP/LP01LSL_SQL.CPY".
       COPY "LIBLP/LPWSLSL.CPY".
       COPY "LIBLP/LP01LSM.CPY".
       COPY "LIBLP/LP01LSM_SQL.CPY".
       COPY "LIBLP/LPWSLSM.CPY".
       COPY "LIBLP/LP01LSP.CPY".
       COPY "LIBLP/LP01LSP_SQL.CPY".
       COPY "LIBLP/LPWSLSP.CPY".
       COPY "LIBLP/LP01LSR.CPY".
       COPY "LIBLP/LP01LSR_SQL.CPY".
       COPY "LIBLP/LPWSLSR.CPY".
       COPY "LIBLP/LP01LSX.CPY".
       COPY "LIBLP/LP01LSX_SQL.CPY".
       COPY "LIBLP/LPWSLSX.CPY".



       01  LEGEND               PIC X(60)         VALUE SPACES.
       01  MESS-LIST            PIC X(5)     VALUE "MESS.".

       01  SAVE-LM1-KEY                 PIC X(23)   VALUE SPACES.
       01  FILLER             REDEFINES SAVE-LM1-KEY.
           03  SAVE-LM-BRNO             PIC 9(4).
           03  SAVE-LM-ACCTNO           PIC 9(8).
           03  SAVE-LM-DATE             PIC 9(8).
           03  SAVE-LM-SEQ              PIC 9(3).

       01  HOLD-LM1-KEY                 PIC X(23).
       01  HOLD-LE1-KEY                 PIC X(31).
       01  HOLD-LP1-KEY                 PIC 9(17).
       01  HOLD-NO1-KEY                 PIC X(12).
       01  HOLD-LX1-KEY                 PIC X(17).
       01  HOLD-LXG1-KEY                PIC X(17).
       01  HOLD-LXE1-KEY                PIC X(17).
       01  HOLD-LS1-KEY                 PIC X(14).
      * PIC OF 9(22) MUST BE TO LARGE, ITS MOVING FROM AN 01 TO AN 01
       01  HOLD-LH1-KEY                 PIC X(22).
       01  HOLD-DT1-KEY                 PIC X(32).
       01  HOLD-LT1-KEY                 PIC X(15).

       01  NEW-ACCTNO                   PIC 9(8).
       01  OLD-ACCTNO                   PIC 9(8).

       COPY "LIBGB/EOBUF_EXT.CPY".
           03  BEG-BRANCH                    PIC 9(4).
           03  OLD-NUMBER                    PIC 9(6).
           03  NEW-NUMBER                    PIC 9(6).
 
       01  VERSION-CONTROL                   PIC X(01)     VALUE "A".

       01  LN-FEED                       PIC 9(02)     VALUE ZEROES.


       COPY "LIBCL/CREAIXW.CPY".

       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBGB/MKTEMPW_CMD.CPY".
       COPY "LIBGB/PASSWDW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBFX/NEWNUM_DEF.CPY".
       COPY "LIBFX/NEWNUM_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/SYSTEMW.CPY".

       COPY "LIBGB/GETFMW.CPY".

       SCREEN SECTION.
       COPY "LIBFX/NEWNUM_SCN.CPY".

      ******************************************************************
      *
      *    P R O C E D U R E - D I V I S I O N
      *
      ******************************************************************
       PROCEDURE DIVISION
             USING FORM-PATHNAME EXIT-PATHNAME.
       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
               PR-FILE.
       COPY "LIBGB/DECLRP.CPY".
           PERFORM CLOSE-CJ1-FILE.
           PERFORM CLOSE-NO1-FILE.
           PERFORM CLOSE-LXG1-FILE.
           PERFORM CLOSE-LXE1-FILE.
           PERFORM CLOSE-LX1-FILE.
           PERFORM CLOSE-LTP1-FILE.
           PERFORM CLOSE-LTL1-FILE.
           PERFORM CLOSE-LTI1-FILE.
           PERFORM CLOSE-LTF1-FILE.
           PERFORM CLOSE-LTC1-FILE.
           PERFORM CLOSE-LTB1-FILE.
           PERFORM CLOSE-LTA1-FILE.
           PERFORM CLOSE-LSX1-FILE.
           PERFORM CLOSE-LSR1-FILE.
           PERFORM CLOSE-LSP1-FILE.
           PERFORM CLOSE-LSM1-FILE.
           PERFORM CLOSE-LSL1-FILE.
           PERFORM CLOSE-LSJ1-FILE.
           PERFORM CLOSE-LSG1-FILE.
           PERFORM CLOSE-LSF1-FILE.
           PERFORM CLOSE-LSD1-FILE.
           PERFORM CLOSE-LP1-FILE.
           PERFORM CLOSE-LN1-FILE.
           PERFORM CLOSE-LM1-FILE.
           PERFORM CLOSE-LH1-FILE.
           PERFORM CLOSE-LE1-FILE.
           PERFORM CLOSE-DT1-FILE.
           PERFORM CLOSE-CK1-FILE.
           PERFORM CLOSE-BW1-FILE.
           PERFORM CLOSE-BR-FILE.
           PERFORM CLOSE-PR-FILE.
       COPY "LIBGB/DECLRP2.CPY".

      ************************
       MAIN-MODULE SECTION.
      ************************

           PERFORM SQL-CONNECT.

      D    STOP MESS.
           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "NEWNUM"      TO VDU-FORM-NAME.

           PERFORM INITIALIZATION.
           IF ( EXT-EOBUF-ERRCD NOT = SPACES )
              GO TO END-PROGRAM.

           MOVE EXT-FILPATH-BASE           TO FILPATH-BASE.
           MOVE BR-MACHINE                 TO FILPATH-MACHINE.
           MOVE BR-DATA-PATH               TO FILPATH-DATA.

           MOVE "B"  TO         DT-PATH-OVERRIDE
                                CK-PATH-OVERRIDE
                                NO-PATH-OVERRIDE
                                LE-PATH-OVERRIDE
                                LH-PATH-OVERRIDE
                                LM-PATH-OVERRIDE
                                LP-PATH-OVERRIDE
                                LTA-PATH-OVERRIDE
                                LTB-PATH-OVERRIDE
                                LTC-PATH-OVERRIDE
                                LTF-PATH-OVERRIDE
                                LTI-PATH-OVERRIDE
                                LTL-PATH-OVERRIDE
                                LTP-PATH-OVERRIDE
                                IX-PATH-OVERRIDE
                                LX-PATH-OVERRIDE
                                LXG-PATH-OVERRIDE
                                LXE-PATH-OVERRIDE
                                LSD-PATH-OVERRIDE
                                LSF-PATH-OVERRIDE
                                LSG-PATH-OVERRIDE
                                LSJ-PATH-OVERRIDE
                                LSL-PATH-OVERRIDE
                                LSM-PATH-OVERRIDE
                                LSP-PATH-OVERRIDE
                                LSR-PATH-OVERRIDE
                                LSX-PATH-OVERRIDE.

           MOVE FILPATH-BASE            TO IX-ALL-BASE.
           MOVE FILPATH-MACHINE         TO IX-ALL-MACHINE.
           MOVE FILPATH-DATA            TO IX-ALL-DATA.
           MOVE IX-ALL-PATH             TO IX-PATH CREAIX-IX-PATH.

           PERFORM LOAN-RENUMBER.


       EXIT-MAIN-MODULE.
           GO TO END-PROGRAM.

      *******************************
       INITIALIZATION SECTION.
      *******************************
           MOVE NEWNUM-QUEUE-POS TO Q-POS.
           MOVE NEWNUM-COPIES-POS TO C-POS.
           MOVE NEWNUM-PRU-POS   TO P-POS.
           MOVE SPACES TO Q-BUF C-BUF P-BUF.
       COPY "LIBGB/EOINIT.CPY".


           IF ( EXT-EOBUF-ERRCD = "E" )
              GO TO END-INITIALIZATION.
 
           GO TO EXIT-INITIALIZE.

       END-INITIALIZATION.

           MOVE "X" TO EXT-EOBUF-ERRCD.

       EXIT-INITIALIZE.
           EXIT.

      ***************************
       EO-EXEC-AUDIT SECTION.
      ***************************

      ***************************
       ENTER-PARAMETERS SECTION.
      ***************************
       BEGIN-ENTER-PARAMETERS.

           IF ( UP-KEY ) GO TO ENTER-NEW-ACCOUNT.

       ENTER-BEG-BRANCH.

           MOVE "EN N040BR1." TO VDU.
           PERFORM SCREENIO.
           IF ( F1-KEY ) GO TO EXIT-ENTER-PARAMETERS.
           IF ( F7-KEY ) GO TO EXIT-ENTER-PARAMETERS.
           IF NOT ( DOWN-KEY OR ENTER-KEY ) GO TO ENTER-BEG-BRANCH.

           MOVE VDUNUM0 TO BEG-BRANCH.
           MOVE BEG-BRANCH TO BR-NO.
           PERFORM OPEN-BR-FILE.
           PERFORM READ-BR-FILE.
           PERFORM CLOSE-BR-FILE.
           IF IO-FG = 9
              MOVE "INVALID BRANCH" TO MESS PERFORM SEND-MESS
              GO TO ENTER-BEG-BRANCH.

           MOVE EXT-FILPATH-BASE           TO FILPATH-BASE.
           MOVE BR-MACHINE                 TO FILPATH-MACHINE.
           MOVE BR-DATA-PATH               TO FILPATH-DATA.

           MOVE "B"  TO         LN-PATH-OVERRIDE
                                BW-PATH-OVERRIDE.

       ENTER-OLD-ACCOUNT.
           MOVE "ENNN060OLDACC." TO VDU.
           PERFORM SCREENIO.
           IF ( F1-KEY ) GO TO EXIT-ENTER-PARAMETERS.
           IF ( F7-KEY ) GO TO EXIT-ENTER-PARAMETERS.
           IF ( UP-KEY ) GO TO ENTER-BEG-BRANCH.
           IF NOT ( DOWN-KEY OR ENTER-KEY )
              GO TO ENTER-OLD-ACCOUNT.

           MOVE VDUNUM0 TO OLD-NUMBER.
           PERFORM OPEN-LN1-FILE.

           MOVE LN-PATH-OWNBR TO LN-OWNBR.
           MOVE OLD-NUMBER    TO LN-ACCTNO.
           PERFORM READ-LN1-FILE.
           IF IO-FG NOT = 0
              MOVE "INVALID LOAN NUMBER" TO MESS PERFORM SEND-MESS
              GO TO ENTER-OLD-ACCOUNT.

           MOVE OLD-NUMBER TO OLD-ACCTNO.

           PERFORM OPEN-BW1-FILE.
           MOVE LN-SSNO(1) TO BW-SSNO.
           PERFORM READ-BW1-FILE.
           PERFORM CLOSE-BW1-FILE.
           IF IO-FG = 0
              MOVE "S  A200OLDNAME." TO VDU
              STRING BW-FNAME," ",BW-LNAME DELIMITED BY "   "
                INTO VDUBUF
              PERFORM SCREENIO.

      *-----------------------------------------------------------------
       ENTER-NEW-ACCOUNT.
           MOVE "ENNN060NEWACC." TO VDU.
           PERFORM SCREENIO.
           IF ( F1-KEY ) GO TO EXIT-ENTER-PARAMETERS.
           IF ( F7-KEY ) GO TO EXIT-ENTER-PARAMETERS.
           IF ( UP-KEY ) GO TO ENTER-OLD-ACCOUNT.
           IF NOT ( DOWN-KEY OR ENTER-KEY )
              GO TO ENTER-NEW-ACCOUNT.

           IF VDUNUM0 = 0
              GO TO ENTER-NEW-ACCOUNT.

           MOVE VDUNUM0 TO NEW-NUMBER.

           IF NEW-NUMBER = OLD-NUMBER
              MOVE "SAME NUMBER" TO MESS PERFORM SEND-MESS
              GO TO ENTER-NEW-ACCOUNT.
           
           PERFORM OPEN-LN1-FILE.
           MOVE LN-PATH-OWNBR TO LN-OWNBR.
           MOVE NEW-NUMBER    TO LN-ACCTNO.
           PERFORM READ-LN1-FILE.
           IF IO-FG = 0
              MOVE "ACCOUNT ALREADY EXISTS" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-NEW-ACCOUNT.

           MOVE NEW-NUMBER TO NEW-ACCTNO.

       ENTER-ACCEPT-YN.

       EXIT-ENTER-PARAMETERS.
           EXIT.

      ***********************************
       DISPLAY-PARAMETERS SECTION.
      ***********************************

       EXIT-DISPLAY-PARAMETERS.
           EXIT.

      ***********************************
       LOAN-RENUMBER SECTION.
      ***********************************

           MOVE LN-PATH-OWNBR TO LN-OWNBR.
           MOVE OLD-NUMBER    TO LN-ACCTNO.
           PERFORM READ-LN1-FILE.
           IF IO-FG = 9
              GO TO EXIT-LOAN-RENUMBER.

           PERFORM RENUMBER-LM-RECORDS.
           PERFORM RENUMBER-LP-RECORDS.
           PERFORM RENUMBER-LX-RECORDS.
           PERFORM RENUMBER-LXG-RECORDS.
           PERFORM RENUMBER-LXE-RECORDS.
           PERFORM RENUMBER-LSD-RECORDS.
           PERFORM RENUMBER-LSF-RECORDS.
           PERFORM RENUMBER-LSG-RECORDS.
           PERFORM RENUMBER-LSJ-RECORDS.
           PERFORM RENUMBER-LSL-RECORDS.
           PERFORM RENUMBER-LSM-RECORDS.
           PERFORM RENUMBER-LSP-RECORDS.
           PERFORM RENUMBER-LSR-RECORDS.
           PERFORM RENUMBER-LSX-RECORDS.
           PERFORM RENUMBER-LTA-RECORDS.
           PERFORM RENUMBER-LTB-RECORDS.
           PERFORM RENUMBER-LTC-RECORDS.
           PERFORM RENUMBER-LTF-RECORDS.
           PERFORM RENUMBER-LTI-RECORDS.
           PERFORM RENUMBER-LTL-RECORDS.
           PERFORM RENUMBER-LTP-RECORDS.
           IF ( LN-SMALLOAN = "Y" )
              PERFORM RENUMBER-LE-RECORDS.

           PERFORM RENUMBER-LH-RECORDS.
           PERFORM RENUMBER-DT-RECORDS.
           PERFORM RENUMBER-CK-RECORDS.
           PERFORM RENUMBER-NO-RECORDS.
           PERFORM RENUMBER-CJ-RECORDS.

           MOVE LN-PATH-OWNBR TO LN-OWNBR.
           MOVE NEW-NUMBER    TO LN-ACCTNO.
           PERFORM WRITE-LN1-FILE.
           IF IO-FG = 9
              GO TO IO-ERROR.

           MOVE LN-PATH-OWNBR TO LN-OWNBR.
           MOVE OLD-ACCTNO    TO LN-ACCTNO.
           PERFORM READ-LN1-FILE.
           IF IO-FG = 0
              PERFORM DELETE-LN1-FILE.

      * PUT A MEMO ON THE NEW ACCOUNT
           PERFORM OPEN-LM1-FILE.

           MOVE SPACES TO LM-REC.

           MOVE LM-PATH-OWNBR TO SAVE-LM-BRNO.
           MOVE NEW-ACCTNO    TO SAVE-LM-ACCTNO.
           MOVE EXT-CONAME-SYSDATE TO ALP-DATE.
           PERFORM CONVERT-ALPDATE-TO-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.
           COMPUTE SAVE-LM-DATE = 99999999 - DATE-YYYYMMDD.
           MOVE 1             TO SAVE-LM-SEQ.
       CREATE-NEW-MEMO.
           MOVE SAVE-LM1-KEY TO LM1-KEY.
           PERFORM READ-LM1-FILE.
           IF IO-FG = 0
              IF LM-SEQ < 999
                 ADD 1 TO SAVE-LM-SEQ
                 GO TO CREATE-NEW-MEMO
              ELSE
                 GO TO EXIT-LOAN-RENUMBER.

           MOVE SPACES TO LM-REC.
           MOVE SAVE-LM1-KEY TO LM1-KEY.

           STRING "ACCOUNT NUMBER CHANGED FROM: ",OLD-NUMBER 
                      DELIMITED BY "  " INTO LM-MEMO.
           PERFORM WRITE-LM1-FILE.
           PERFORM CLOSE-LM1-FILE.
           
       EXIT-LOAN-RENUMBER.
           EXIT.

      *******************************
       RENUMBER-LM-RECORDS SECTION.
      *******************************
           PERFORM OPEN-LM1-FILE.

       START-RENUMBER-LM.
      * OLD NUMBER
           MOVE LM-PATH-OWNBR TO LM-BRNO
                                 QLM1-WBEG-BRNO
                                 QLM1-WEND-BRNO.
           MOVE LN-ACCTNO     TO LM-ACCTNO
                                 QLM1-WBEG-ACCTNO
                                 QLM1-WEND-ACCTNO.
           MOVE 0             TO LM-DATE-REVERSED LM-SEQ
                                 QLM1-WBEG-DATE-REV
                                 QLM1-WBEG-SEQ.
           MOVE ALL "9"       TO QLM1-WEND-DATE-REV
                                 QLM1-WEND-SEQ.

           PERFORM START-LM1-FILE.

       NEXT-MEMO.
           PERFORM READ-LM1-FILE-NEXT.
           IF ( IO-FG NOT = 0 ) OR ( LM-BRNO NOT = LM-PATH-OWNBR)
                                OR ( LM-ACCTNO NOT = LN-ACCTNO )
              GO TO EXIT-RENUMBER-LM.

           MOVE LM1-KEY TO HOLD-LM1-KEY.

      * WRITE NEW MEMO
           MOVE NEW-ACCTNO TO LM-ACCTNO.
           PERFORM WRITE-LM1-FILE.
           IF IO-FG NOT = 0
              PERFORM REWRITE-LM1-FILE.

      * READ BACK IN OLD MEMO
           MOVE HOLD-LM1-KEY TO LM1-KEY.
           PERFORM READ-LM1-FILE.
           IF IO-FG = 0
              PERFORM DELETE-LM1-FILE.

           GO TO START-RENUMBER-LM.

       EXIT-RENUMBER-LM.
           PERFORM CLOSE-LM1-FILE.

      ***************************************
       RENUMBER-LE-RECORDS SECTION.
      ***************************************
           PERFORM OPEN-LE1-FILE.

       START-RENUMBER-LE.
           MOVE SPACES    TO LE-REC.
      * OLD NUMBER
           MOVE LN-OWNBR      TO LE-BRNO
                                 QLE1-WBEG-BRNO
                                 QLE1-WEND-BRNO.
           MOVE LN-ACCTNO     TO LE-ACCTNO
                                 QLE1-WBEG-ACCTNO
                                 QLE1-WEND-ACCTNO.
           MOVE "E"           TO LE-TYPE
                                 QLE1-WBEG-TYPE.
           MOVE SPACES        TO LE-FLAG
                                 QLE1-WBEG-FLAG.
           MOVE "Z"           TO QLE1-WEND-TYPE
                                 QLE1-WEND-FLAG.
           MOVE 0             TO LE-SSNO LE-OSACCTNO
                                 QLE1-WBEG-SSNO
                                 QLE1-WBEG-OSACCTNO.
           MOVE ALL "9"       TO QLE1-WEND-SSNO
                                 QLE1-WEND-OSACCTNO.

           PERFORM START-LE1-FILE.

       NEXT-ENDORSER.
           PERFORM READ-LE1-FILE-NEXT.
           IF ( IO-FG NOT = 0 ) OR (LE-BRNO NOT = LN-OWNBR)  
              GO TO EXIT-RENUMBER-LE.

           IF ( LE-ACCTNO NOT = LN-ACCTNO )
      *       UNLOCK LE-FILE
              GO TO EXIT-RENUMBER-LE.

           MOVE LE1-KEY TO HOLD-LE1-KEY.

      * WRITE NEW ACCOUNT
           MOVE NEW-ACCTNO TO LE-ACCTNO.
           PERFORM WRITE-LE1-FILE.
           IF IO-FG NOT = 0
              PERFORM REWRITE-LE1-FILE.
           
      * READ BACK IN OLD RECORD
           MOVE HOLD-LE1-KEY TO LE1-KEY.
           PERFORM READ-LE1-FILE.
           IF IO-FG = 0
              PERFORM DELETE-LE1-FILE.

           GO TO START-RENUMBER-LE.

       EXIT-RENUMBER-LE.
           PERFORM CLOSE-LE1-FILE.


      *********************************
       RENUMBER-LP-RECORDS SECTION.
      *********************************

           PERFORM OPEN-LP1-FILE.

       START-RENUMBER-LP.
           MOVE LP-PATH-OWNBR TO LP-BRNO
                                 QLP1-WBEG-BRNO
                                 QLP1-WEND-BRNO.
           MOVE LN-ACCTNO     TO LP-ACCTNO
                                 QLP1-WBEG-ACCTNO
                                 QLP1-WEND-ACCTNO.
           MOVE 0             TO LP-SEQNO
                                 QLP1-WBEG-SEQNO.
           MOVE ALL "9"       TO QLP1-WEND-SEQNO.

           PERFORM START-LP1-FILE.

       NEXT-PAYMENT.
           PERFORM READ-LP1-FILE-NEXT.
           IF IO-BAD OR (LP-BRNO NOT = LP-PATH-OWNBR)
                      OR (LP-ACCTNO NOT = LN-ACCTNO)
      *       UNLOCK LP-FILE
              GO TO EXIT-RENUMBER-LP.

           MOVE LP1-KEY TO HOLD-LP1-KEY.

      * WRITE NEW
           MOVE NEW-ACCTNO TO LP-ACCTNO.
           PERFORM WRITE-LP1-FILE.
           IF IO-FG NOT = 0
              PERFORM REWRITE-LP1-FILE.

      * READ BACK IN OLD ONE
            MOVE HOLD-LP1-KEY TO LP1-KEY.
            PERFORM READ-LP1-FILE.
            IF IO-FG = 0
              PERFORM DELETE-LP1-FILE.

           GO TO START-RENUMBER-LP.

       EXIT-RENUMBER-LP.
           PERFORM CLOSE-LP1-FILE.

      *****************************
       RENUMBER-LX-RECORDS SECTION.
      *****************************
           PERFORM OPEN-LX1-FILE.


       START-RENUMBER-LX.
           MOVE LN-OWNBR  TO LX-BRNO
                             QLX1-WBEG-BRNO
                             QLX1-WEND-BRNO.
           MOVE LN-ACCTNO TO LX-ACCTNO
                             QLX1-WBEG-ACCTNO
                             QLX1-WEND-ACCTNO.
           MOVE 0         TO LX-SEQNO
                             QLX1-WBEG-SEQNO.
           MOVE ALL "9"   TO QLX1-WEND-SEQNO.

           PERFORM START-LX1-FILE.

       RENUMBER-LX-NEXT.
           PERFORM READ-LX1-FILE-NEXT.
           IF ( IO-FG NOT = 0 )
              GO TO EXIT-RENUMBER-LX.

           IF (LX-ACCTNO NOT = LN-ACCTNO) OR (LX-BRNO NOT = LN-OWNBR)
      *       UNLOCK LX-FILE
              GO TO EXIT-RENUMBER-LX.

           MOVE LX1-KEY TO HOLD-LX1-KEY.

           MOVE NEW-ACCTNO TO LX-ACCTNO.
           PERFORM WRITE-LX1-FILE.
           IF IO-FG NOT = 0
              PERFORM REWRITE-LX1-FILE.

           MOVE HOLD-LX1-KEY TO LX1-KEY.
           PERFORM READ-LX1-FILE.
           IF IO-FG = 0
              PERFORM DELETE-LX1-FILE.

           GO TO START-RENUMBER-LX.

       EXIT-RENUMBER-LX.
           PERFORM CLOSE-LX1-FILE.

      *****************************
       RENUMBER-LXG-RECORDS SECTION.
      *****************************
           PERFORM OPEN-LXG1-FILE.


       START-RENUMBER-LXG.

           MOVE LN-OWNBR  TO LXG-BRNO
                             QLXG1-WBEG-BRNO
                             QLXG1-WEND-BRNO.
           MOVE LN-ACCTNO TO LXG-ACCTNO
                             QLXG1-WBEG-ACCTNO
                             QLXG1-WEND-ACCTNO.
           MOVE 0         TO LXG-SEQNO
                             QLXG1-WBEG-SEQNO.
           MOVE ALL "9"   TO QLXG1-WEND-SEQNO.

           PERFORM START-LXG1-FILE.

       RENUMBER-LXG-NEXT.
           PERFORM READ-LXG1-FILE-NEXT.
           IF ( IO-FG NOT = 0 )
              GO TO EXIT-RENUMBER-LXG.

           IF (LXG-ACCTNO NOT = LN-ACCTNO) OR (LXG-BRNO NOT = LN-OWNBR)
      *       UNLOCK LXG-FILE
              GO TO EXIT-RENUMBER-LXG.

           MOVE LXG1-KEY TO HOLD-LXG1-KEY.

           MOVE NEW-ACCTNO TO LXG-ACCTNO.
           PERFORM WRITE-LXG1-FILE.
           IF IO-FG NOT = 0
              PERFORM REWRITE-LXG1-FILE.

           MOVE HOLD-LXG1-KEY TO LXG1-KEY.
           PERFORM READ-LXG1-FILE.
           IF IO-FG = 0
              PERFORM DELETE-LXG1-FILE.

           GO TO START-RENUMBER-LXG.

       EXIT-RENUMBER-LXG.
           PERFORM CLOSE-LXG1-FILE.

      *****************************
       RENUMBER-LXE-RECORDS SECTION.
      *****************************
           PERFORM OPEN-LXE1-FILE.


       START-RENUMBER-LXE.

           MOVE LN-OWNBR  TO LXE-BRNO
                             QLXE1-WBEG-BRNO
                             QLXE1-WEND-BRNO.
           MOVE LN-ACCTNO TO LXE-ACCTNO
                             QLXE1-WBEG-ACCTNO
                             QLXE1-WEND-ACCTNO.
           MOVE 0         TO LXE-SEQNO
                             QLXE1-WBEG-SEQNO.
           MOVE ALL "9"   TO QLXE1-WEND-SEQNO.

           PERFORM START-LXE1-FILE.

       RENUMBER-LXE-NEXT.
           PERFORM READ-LXE1-FILE-NEXT.
           IF ( IO-FG NOT = 0 )
              GO TO EXIT-RENUMBER-LXE.

           IF (LXE-ACCTNO NOT = LN-ACCTNO) OR (LXE-BRNO NOT = LN-OWNBR)
      *       UNLOCK LXE-FILE
              GO TO EXIT-RENUMBER-LXE.

           MOVE LXE1-KEY TO HOLD-LXE1-KEY.

           MOVE NEW-ACCTNO TO LXE-ACCTNO.
           PERFORM WRITE-LXE1-FILE.
           IF IO-FG NOT = 0
              PERFORM REWRITE-LXE1-FILE.

           MOVE HOLD-LXE1-KEY TO LXE1-KEY.
           PERFORM READ-LXE1-FILE.
           IF IO-FG = 0
              PERFORM DELETE-LXE1-FILE.

           GO TO START-RENUMBER-LXE.

       EXIT-RENUMBER-LXE.
           PERFORM CLOSE-LXE1-FILE.

      *****************************
       RENUMBER-LH-RECORDS SECTION.
      *****************************
           PERFORM OPEN-LH1-FILE.

           MOVE 0             TO LH-SSNO LH-ACCTNO.
           
           MOVE LH-PATH-OWNBR TO LH-BRNO
                                 QLH1-WBEG-BRNO
                                 QLH1-WEND-BRNO.
           MOVE 0             TO LH-SSNO
                                 QLH1-WBEG-SSNO.
           MOVE ALL "9"       TO QLH1-WEND-SSNO.
           MOVE LN-ACCTNO     TO LH-ACCTNO
                                 QLH1-WBEG-ACCTNO
                                 QLH1-WEND-ACCTNO.

       START-RENUMBER-LH.
           PERFORM START-LH1-FILE.

       RENUMBER-LH-NEXT.
           PERFORM READ-LH1-FILE-NEXT.
           IF ( IO-FG NOT = 0 ) OR ( LH-BRNO NOT = LH-PATH-OWNBR )
              GO TO EXIT-RENUMBER-LH.

           IF LH-ACCTNO NOT = LN-ACCTNO
      *       UNLOCK LH-FILE
              GO TO RENUMBER-LH-NEXT.

           MOVE LH1-KEY TO HOLD-LH1-KEY.

           MOVE NEW-ACCTNO TO LH-ACCTNO.
           PERFORM WRITE-LH1-FILE.
           IF IO-FG NOT = 0
              PERFORM REWRITE-LH1-FILE.

           MOVE HOLD-LH1-KEY TO LH1-KEY.
           PERFORM READ-LH1-FILE.
           IF IO-FG = 0
              PERFORM DELETE-LH1-FILE.

           MOVE HOLD-LH1-KEY TO LH1-KEY.
           GO TO START-RENUMBER-LH.

       EXIT-RENUMBER-LH.
           PERFORM CLOSE-LH1-FILE.

      *****************************
       RENUMBER-CK-RECORDS SECTION.
      *****************************
           PERFORM OPEN-CK1-FILE.

           MOVE CK-PATH-OWNBR TO CK-BRNO.
           MOVE 0             TO CK-NO.

           PERFORM START-CK1-FILE.

       RENUMBER-CK-NEXT.
           PERFORM READ-CK1-FILE-NEXT.
           IF IO-BAD OR (CK-BRNO NOT = CK-PATH-OWNBR)
      *       UNLOCK CK-FILE
              GO TO EXIT-RENUMBER-CK.

           IF (CK-ACCTNO NOT = LN-ACCTNO)
      *       UNLOCK CK-FILE
              GO TO RENUMBER-CK-NEXT.

           MOVE NEW-ACCTNO TO CK-ACCTNO.
           PERFORM REWRITE-CK1-FILE.

           GO TO RENUMBER-CK-NEXT.

       EXIT-RENUMBER-CK.
           PERFORM CLOSE-CK1-FILE.

      *****************************
       RENUMBER-CJ-RECORDS SECTION.
      *****************************
           PERFORM OPEN-CJ1-FILE.

           MOVE CJ-PATH-OWNBR TO CJ-BRNO.
           MOVE 19000101      TO CJ-DATE.
           MOVE 0             TO CJ-START-TIME CJ-SEQNO.
           MOVE SPACES        TO CJ-COLLID.

           PERFORM START-CJ1-FILE.

       RENUMBER-CJ-NEXT.
           PERFORM READ-CJ1-FILE-NEXT.
           IF ( IO-FG NOT = 0 )
              GO TO EXIT-RENUMBER-CJ.

           IF (CJ-BRNO NOT = CJ-PATH-OWNBR) OR
                  (CJ-ACCTNO NOT = LN-ACCTNO)
      *       UNLOCK CJ-FILE
              GO TO RENUMBER-CJ-NEXT.

           MOVE NEW-ACCTNO TO CJ-ACCTNO.
           PERFORM REWRITE-CJ1-FILE.

           GO TO RENUMBER-CJ-NEXT.

       EXIT-RENUMBER-CJ.
           PERFORM CLOSE-CJ1-FILE.

      *****************************
       RENUMBER-DT-RECORDS SECTION.
      *****************************
           PERFORM OPEN-DT1-FILE.

           MOVE DT-PATH-OWNBR TO DT-BRNO
                                 QDT1-WBEG-BRNO
                                 QDT1-WEND-BRNO.
           MOVE LN-ACCTNO     TO DT-ACCTNO 
                                 QDT1-WBEG-ACCTNO
                                 QDT1-WEND-ACCTNO.
           MOVE 0             TO DT-NUM DT-CLASS DT-SEQ
                                 QDT1-WBEG-NUM
                                 QDT1-WBEG-CLASS
                                 QDT1-WBEG-SEQ.
           MOVE ALL "9"       TO QDT1-WEND-NUM
                                 QDT1-WEND-CLASS
                                 QDT1-WEND-SEQ.
           MOVE 19000101      TO DT-DATE SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QDT1-WBEG-DATE.
           MOVE EXT-JULIAN-END      TO SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD  TO QDT1-WEND-DATE.

       START-RENUMBER-DT.
           PERFORM START-DT1-FILE.

       RENUMBER-DT-NEXT.
           PERFORM READ-DT1-FILE-NEXT.
            IF ( IO-FG NOT = 0) OR (DT-BRNO NOT = DT-PATH-OWNBR)
              GO TO EXIT-RENUMBER-DT.

           IF (DT-ACCTNO NOT = LN-ACCTNO)
      *       UNLOCK DT-FILE
              GO TO RENUMBER-DT-NEXT.

           MOVE DT1-KEY TO HOLD-DT1-KEY.

           MOVE NEW-ACCTNO TO DT-ACCTNO.
           PERFORM WRITE-DT1-FILE.
           IF IO-FG NOT = 0
              PERFORM REWRITE-DT1-FILE.

           MOVE HOLD-DT1-KEY TO DT1-KEY.
           PERFORM READ-DT1-FILE.
           IF IO-FG = 0
              PERFORM DELETE-DT1-FILE.

           MOVE HOLD-DT1-KEY TO DT1-KEY.
           GO TO START-RENUMBER-DT.

       EXIT-RENUMBER-DT.
           PERFORM CLOSE-DT1-FILE.

      *********************************
       RENUMBER-NO-RECORDS SECTION.
      *********************************
            PERFORM OPEN-NO1-FILE.

           MOVE NO-PATH-OWNBR TO NO-BRNO.
           MOVE LN-ACCTNO     TO NO-ACCTNO.
           PERFORM READ-NO1-FILE.
           IF ( IO-FG NOT = 0 ) 
              GO TO EXIT-RENUMBER-NO.

           MOVE NO1-KEY TO HOLD-NO1-KEY.

           MOVE NEW-ACCTNO TO NO-ACCTNO.
           PERFORM WRITE-NO1-FILE.
           IF IO-FG NOT = 0
              PERFORM REWRITE-NO1-FILE.

           MOVE HOLD-NO1-KEY TO NO1-KEY.
           PERFORM READ-NO1-FILE.
           IF IO-FG = 0
              PERFORM DELETE-NO1-FILE.

       EXIT-RENUMBER-NO.
           PERFORM CLOSE-NO1-FILE.

      *******************************
       RENUMBER-LSD-RECORDS SECTION.
      *******************************
           PERFORM OPEN-LSD1-FILE.


       START-RENUMBER-LSD.

           MOVE LSD-PATH-OWNBR TO LSD-BRNO
                                  QLSD1-WBEG-BRNO
                                  QLSD1-WEND-BRNO.
           MOVE LN-ACCTNO      TO LSD-ACCTNO
                                  QLSD1-WBEG-ACCTNO
                                  QLSD1-WEND-ACCTNO.
           MOVE 0              TO LSD-SEQNO
                                  QLSD1-WBEG-SEQNO.
           MOVE ALL "9"        TO QLSD1-WEND-SEQNO.

           PERFORM START-LSD1-FILE.
       RENUMBER-LSD-NEXT.
           PERFORM READ-LSD1-FILE-NEXT.
           IF ( IO-FG NOT = 0 )
              GO TO EXIT-RENUMBER-LSD.

           IF (LSD-BRNO NOT = LSD-PATH-OWNBR) 
                OR (LSD-ACCTNO NOT = LN-ACCTNO)
      *       UNLOCK LSD-FILE
              GO TO EXIT-RENUMBER-LSD.

           MOVE LSD1-KEY TO HOLD-LS1-KEY.

           MOVE NEW-ACCTNO TO LSD-ACCTNO.
           PERFORM WRITE-LSD1-FILE.
           IF IO-FG NOT = 0
              PERFORM REWRITE-LSD1-FILE.

           MOVE HOLD-LS1-KEY TO LSD1-KEY.
           PERFORM READ-LSD1-FILE.
           IF IO-FG = 0
              PERFORM DELETE-LSD1-FILE.

           GO TO START-RENUMBER-LSD.

       EXIT-RENUMBER-LSD.
           PERFORM CLOSE-LSD1-FILE.

      *******************************
       RENUMBER-LSF-RECORDS SECTION.
      *******************************
           PERFORM OPEN-LSF1-FILE.


       START-RENUMBER-LSF.
           MOVE LSF-PATH-OWNBR TO LSF-BRNO
                                  QLSF1-WBEG-BRNO
                                  QLSF1-WEND-BRNO.
           MOVE LN-ACCTNO      TO LSF-ACCTNO
                                  QLSF1-WBEG-ACCTNO
                                  QLSF1-WEND-ACCTNO.
           MOVE 0              TO LSF-SEQNO
                                  QLSF1-WBEG-SEQNO.
           MOVE ALL "9"        TO QLSF1-WEND-SEQNO.

           PERFORM START-LSF1-FILE.
       RENUMBER-LSF-NEXT.
           PERFORM READ-LSF1-FILE-NEXT.
           IF ( IO-FG NOT = 0 )
              GO TO EXIT-RENUMBER-LSF.

           IF (LSF-BRNO NOT = LSF-PATH-OWNBR)
                  OR (LSF-ACCTNO NOT = LN-ACCTNO)
      *       UNLOCK LSF-FILE
              GO TO EXIT-RENUMBER-LSF.

           MOVE LSF1-KEY TO HOLD-LS1-KEY.

           MOVE NEW-ACCTNO TO LSF-ACCTNO.
           PERFORM WRITE-LSF1-FILE.
           IF IO-FG NOT = 0
              PERFORM REWRITE-LSF1-FILE.

           MOVE HOLD-LS1-KEY TO LSF1-KEY.
           PERFORM READ-LSF1-FILE.
           IF IO-FG = 0
              PERFORM DELETE-LSF1-FILE.

           GO TO START-RENUMBER-LSF.

       EXIT-RENUMBER-LSF.
           PERFORM CLOSE-LSF1-FILE.

      *******************************
       RENUMBER-LSG-RECORDS SECTION.
      *******************************
           PERFORM OPEN-LSG1-FILE.


       START-RENUMBER-LSG.

           MOVE LSG-PATH-OWNBR  TO LSG-BRNO
                                   QLSG1-WBEG-BRNO
                                   QLSG1-WEND-BRNO.
           MOVE LN-ACCTNO       TO LSG-ACCTNO
                                   QLSG1-WBEG-ACCTNO
                                   QLSG1-WEND-ACCTNO.
           MOVE 0               TO LSG-SEQNO
                                   QLSG1-WBEG-SEQNO.
           MOVE ALL "9"         TO QLSG1-WEND-SEQNO.

           PERFORM START-LSG1-FILE.
       RENUMBER-LSG-NEXT.
           PERFORM READ-LSG1-FILE-NEXT.
           IF ( IO-FG NOT = 0 ) OR (LSG-BRNO NOT = LSG-PATH-OWNBR)
              GO TO EXIT-RENUMBER-LSG.

           IF LSG-ACCTNO NOT = LN-ACCTNO
      *       UNLOCK LSG-FILE
              GO TO EXIT-RENUMBER-LSG.

           MOVE LSG1-KEY TO HOLD-LS1-KEY.

           MOVE NEW-ACCTNO TO LSG-ACCTNO.
           PERFORM WRITE-LSG1-FILE.
           IF IO-FG NOT = 0
              PERFORM REWRITE-LSG1-FILE.

           MOVE HOLD-LS1-KEY TO LSG1-KEY.
           PERFORM READ-LSG1-FILE.
           IF IO-FG = 0
              PERFORM DELETE-LSG1-FILE.

           GO TO START-RENUMBER-LSG.

       EXIT-RENUMBER-LSG.
           PERFORM CLOSE-LSG1-FILE.

      *******************************
       RENUMBER-LSJ-RECORDS SECTION.
      *******************************
           PERFORM OPEN-LSJ1-FILE.



       START-RENUMBER-LSJ.

           MOVE LSJ-PATH-OWNBR  TO LSJ-BRNO
                                   QLSJ1-WBEG-BRNO
                                   QLSJ1-WEND-BRNO.
           MOVE LN-ACCTNO       TO LSJ-ACCTNO
                                   QLSJ1-WBEG-ACCTNO
                                   QLSJ1-WEND-ACCTNO.
           MOVE 0               TO LSJ-SEQNO
                                   QLSJ1-WBEG-SEQNO.
           MOVE ALL "9"         TO QLSJ1-WEND-SEQNO.

           PERFORM START-LSJ1-FILE.
       RENUMBER-LSJ-NEXT.
           PERFORM READ-LSJ1-FILE-NEXT.
           IF ( IO-FG NOT = 0 ) OR (LSJ-BRNO NOT = LSJ-PATH-OWNBR)
              GO TO EXIT-RENUMBER-LSJ.

           IF LSJ-ACCTNO NOT = LN-ACCTNO
      *       UNLOCK LSJ-FILE
              GO TO EXIT-RENUMBER-LSJ.

           MOVE LSJ1-KEY TO HOLD-LS1-KEY.

           MOVE NEW-ACCTNO TO LSJ-ACCTNO.
           PERFORM WRITE-LSJ1-FILE.
           IF IO-FG NOT = 0
              PERFORM REWRITE-LSJ1-FILE.

           MOVE HOLD-LS1-KEY TO LSJ1-KEY.
           PERFORM READ-LSJ1-FILE.
           IF IO-FG = 0
              PERFORM DELETE-LSJ1-FILE.

           GO TO START-RENUMBER-LSJ.

       EXIT-RENUMBER-LSJ.
           PERFORM CLOSE-LSD1-FILE.

      *******************************
       RENUMBER-LSL-RECORDS SECTION.
      *******************************
           PERFORM OPEN-LSL1-FILE.


       START-RENUMBER-LSL.
           MOVE LN-ACCTNO       TO LSL-ACCTNO.
           MOVE 0               TO LSL-SEQNO.

           MOVE LSL-PATH-OWNBR  TO LSL-BRNO
                                   QLSL1-WBEG-BRNO
                                   QLSL1-WEND-BRNO.
           MOVE LN-ACCTNO       TO LSL-ACCTNO
                                   QLSL1-WBEG-ACCTNO
                                   QLSL1-WEND-ACCTNO.
           MOVE 0               TO LSL-SEQNO
                                   QLSL1-WBEG-SEQNO.
           MOVE ALL "9"         TO QLSL1-WEND-SEQNO.

           PERFORM START-LSL1-FILE.
       RENUMBER-LSL-NEXT.
           PERFORM READ-LSL1-FILE-NEXT.
           IF ( IO-FG NOT = 0 ) OR (LSL-BRNO NOT = LSL-PATH-OWNBR)
              GO TO EXIT-RENUMBER-LSL.
           
           IF LSL-ACCTNO NOT = LN-ACCTNO
      *       UNLOCK LSL-FILE
              GO TO EXIT-RENUMBER-LSL.

           MOVE LSL1-KEY TO HOLD-LS1-KEY.

           MOVE NEW-ACCTNO TO LSL-ACCTNO.
           PERFORM WRITE-LSL1-FILE.
           IF IO-FG NOT = 0
              PERFORM REWRITE-LSL1-FILE.

           MOVE HOLD-LS1-KEY TO LSL1-KEY.
           PERFORM READ-LSL1-FILE.
           IF IO-FG = 0
              PERFORM DELETE-LSL1-FILE.

           GO TO START-RENUMBER-LSL.

       EXIT-RENUMBER-LSL.
           PERFORM CLOSE-LSL1-FILE.

      *******************************
       RENUMBER-LSM-RECORDS SECTION.
      *******************************
           PERFORM OPEN-LSM1-FILE.


       START-RENUMBER-LSM.
           MOVE LSM-PATH-OWNBR  TO LSM-BRNO
                                   QLSM1-WBEG-BRNO
                                   QLSM1-WEND-BRNO.
           MOVE LN-ACCTNO       TO LSM-ACCTNO
                                   QLSM1-WBEG-ACCTNO
                                   QLSM1-WEND-ACCTNO.
           MOVE 0               TO LSM-SEQNO
                                   QLSM1-WBEG-SEQNO.
           MOVE ALL "9"         TO QLSM1-WEND-SEQNO.

           PERFORM START-LSM1-FILE.
       RENUMBER-LSM-NEXT.
           PERFORM READ-LSM1-FILE-NEXT.
           IF ( IO-FG NOT = 0 ) OR (LSM-BRNO NOT = LSM-PATH-OWNBR)
      *       UNLOCK LSM-FILE
              GO TO EXIT-RENUMBER-LSM.

           IF LSM-ACCTNO NOT = LN-ACCTNO
      *       UNLOCK LSM-FILE
              GO TO EXIT-RENUMBER-LSM.

           MOVE LSM1-KEY TO HOLD-LS1-KEY.

           MOVE NEW-ACCTNO TO LSM-ACCTNO.
           PERFORM WRITE-LSM1-FILE.
           IF IO-FG NOT = 0
              PERFORM REWRITE-LSM1-FILE.

           MOVE HOLD-LS1-KEY TO LSM1-KEY.
           PERFORM READ-LSM1-FILE.
           IF IO-FG = 0
              PERFORM DELETE-LSM1-FILE.

           GO TO START-RENUMBER-LSM.

       EXIT-RENUMBER-LSM.
           PERFORM CLOSE-LSM1-FILE.

      *******************************
       RENUMBER-LSP-RECORDS SECTION.
      *******************************
           PERFORM OPEN-LSP1-FILE.


       START-RENUMBER-LSP.
           MOVE LSP-PATH-OWNBR  TO LSP-BRNO
                                   QLSP1-WBEG-BRNO
                                   QLSP1-WEND-BRNO.
           MOVE LN-ACCTNO       TO LSP-ACCTNO
                                   QLSP1-WBEG-ACCTNO
                                   QLSP1-WEND-ACCTNO.
           MOVE 0               TO LSP-SEQNO
                                   QLSP1-WBEG-SEQNO.
           MOVE ALL "9"         TO QLSP1-WEND-SEQNO.

           PERFORM START-LSP1-FILE.
       RENUMBER-LSP-NEXT.
           PERFORM READ-LSP1-FILE-NEXT.
           IF ( IO-FG NOT = 0 ) OR (LSP-BRNO NOT = LSP-PATH-OWNBR)
                                 OR (LSP-ACCTNO NOT = LN-ACCTNO)
      *       UNLOCK LSP-FILE
              GO TO EXIT-RENUMBER-LSP.

           MOVE LSP1-KEY TO HOLD-LS1-KEY.

           MOVE NEW-ACCTNO TO LSP-ACCTNO.
           PERFORM WRITE-LSP1-FILE.
           IF IO-FG NOT = 0
              PERFORM REWRITE-LSP1-FILE.

           MOVE HOLD-LS1-KEY TO LSP1-KEY.
           PERFORM READ-LSP1-FILE.
           IF IO-FG = 0
              PERFORM DELETE-LSP1-FILE.

           GO TO START-RENUMBER-LSP.

       EXIT-RENUMBER-LSP.
           PERFORM CLOSE-LSP1-FILE.

      *******************************
       RENUMBER-LSR-RECORDS SECTION.
      *******************************
           PERFORM OPEN-LSR1-FILE.



       START-RENUMBER-LSR.
           MOVE LSR-PATH-OWNBR  TO LSR-BRNO
                                   QLSR1-WBEG-BRNO
                                   QLSR1-WEND-BRNO.
           MOVE LN-ACCTNO       TO LSR-ACCTNO
                                   QLSR1-WBEG-ACCTNO
                                   QLSR1-WEND-ACCTNO.
           MOVE 0               TO LSR-SEQNO
                                   QLSR1-WBEG-SEQNO.
           MOVE ALL "9"         TO QLSR1-WEND-SEQNO.

           PERFORM START-LSR1-FILE.
       RENUMBER-LSR-NEXT.
           PERFORM READ-LSR1-FILE-NEXT.
           IF ( IO-FG NOT = 0 )
              GO TO EXIT-RENUMBER-LSR.

           IF (LSR-BRNO NOT = LSR-PATH-OWNBR) OR 
                (LSR-ACCTNO NOT = LN-ACCTNO)
      *       UNLOCK LSR-FILE
              GO TO EXIT-RENUMBER-LSR.

           MOVE LSR1-KEY TO HOLD-LS1-KEY.

           MOVE NEW-ACCTNO TO LSR-ACCTNO.
           PERFORM WRITE-LSR1-FILE.
           IF IO-FG NOT = 0
              PERFORM REWRITE-LSR1-FILE.

           MOVE HOLD-LS1-KEY TO LSR1-KEY.
           PERFORM READ-LSR1-FILE.
           IF IO-FG = 0
              PERFORM DELETE-LSR1-FILE.

           GO TO START-RENUMBER-LSR.

       EXIT-RENUMBER-LSR.
           PERFORM CLOSE-LSR1-FILE.

      *******************************
       RENUMBER-LSX-RECORDS SECTION.
      *******************************
           PERFORM OPEN-LSX1-FILE.



       START-RENUMBER-LSX.
           MOVE LSX-PATH-OWNBR  TO LSX-BRNO
                                   QLSX1-WBEG-BRNO
                                   QLSX1-WEND-BRNO.
           MOVE LN-ACCTNO       TO LSX-ACCTNO
                                   QLSX1-WBEG-ACCTNO
                                   QLSX1-WEND-ACCTNO.
           MOVE 0               TO LSX-SEQNO
                                   QLSX1-WBEG-SEQNO.
           MOVE ALL "9"         TO QLSX1-WEND-SEQNO.

           PERFORM START-LSX1-FILE.
       RENUMBER-LSX-NEXT.
           PERFORM READ-LSX1-FILE-NEXT.
           IF ( IO-FG NOT = 0 )
              GO TO EXIT-RENUMBER-LSX.

           IF (LSX-BRNO NOT = LSX-PATH-OWNBR) 
              OR (LSX-ACCTNO NOT = LN-ACCTNO)
      *       UNLOCK LSX-FILE
              GO TO EXIT-RENUMBER-LSX.

           MOVE LSX1-KEY TO HOLD-LS1-KEY.

           MOVE NEW-ACCTNO TO LSX-ACCTNO.
           PERFORM WRITE-LSX1-FILE.
           IF IO-FG NOT = 0
              PERFORM REWRITE-LSX1-FILE.

           MOVE HOLD-LS1-KEY TO LSX1-KEY.
           PERFORM READ-LSX1-FILE.
           IF IO-FG = 0
              PERFORM DELETE-LSX1-FILE.

           GO TO START-RENUMBER-LSX.

       EXIT-RENUMBER-LSX.
           PERFORM CLOSE-LSX1-FILE.

      ***********************************
       RENUMBER-LTA-RECORDS SECTION.
      ***********************************
           PERFORM OPEN-LTA1-FILE.

           MOVE LTA-PATH-OWNBR  TO LTA-BRNO.
           MOVE LN-ACCTNO TO LTA-ACCTNO.
           MOVE 1         TO LTA-SEQNO.
           PERFORM READ-LTA1-FILE.
           IF IO-FG NOT = 0 
              GO TO EXIT-RENUMBER-LTA.

           MOVE LTA1-KEY TO HOLD-LT1-KEY.

           MOVE NEW-ACCTNO TO LTA-ACCTNO.
           PERFORM WRITE-LTA1-FILE.
           IF IO-FG NOT = 0
              PERFORM REWRITE-LTA1-FILE.

           MOVE HOLD-LT1-KEY TO LTA1-KEY.
           PERFORM READ-LTA1-FILE.
           IF IO-FG = 0
              PERFORM DELETE-LTA1-FILE.

       EXIT-RENUMBER-LTA.
           PERFORM CLOSE-LTA1-FILE.
     
      ***********************************
       RENUMBER-LTB-RECORDS SECTION.
      ***********************************
           PERFORM OPEN-LTB1-FILE.

           MOVE LTA-PATH-OWNBR  TO LTB-BRNO.
           MOVE LN-ACCTNO TO LTB-ACCTNO.
           MOVE 1         TO LTB-SEQNO.
           PERFORM READ-LTB1-FILE.
           IF IO-FG NOT = 0 
              GO TO EXIT-RENUMBER-LTB.

           MOVE LTB1-KEY TO HOLD-LT1-KEY.

           MOVE NEW-ACCTNO TO LTB-ACCTNO.
           PERFORM WRITE-LTB1-FILE.
           IF IO-FG NOT = 0
              PERFORM REWRITE-LTB1-FILE.

           MOVE HOLD-LT1-KEY TO LTB1-KEY.
           PERFORM READ-LTB1-FILE.
           IF IO-FG = 0
              PERFORM DELETE-LTB1-FILE.

       EXIT-RENUMBER-LTB.
           PERFORM CLOSE-LTB1-FILE.
     
     
      ***********************************
       RENUMBER-LTC-RECORDS SECTION.
      ***********************************
           PERFORM OPEN-LTC1-FILE.

           MOVE LTC-PATH-OWNBR  TO LTC-BRNO.
           MOVE LN-ACCTNO TO LTC-ACCTNO.
           MOVE 1         TO LTC-SEQNO.
           PERFORM READ-LTC1-FILE.
           IF IO-FG NOT = 0 
              GO TO EXIT-RENUMBER-LTC.

           MOVE LTC1-KEY TO HOLD-LT1-KEY.

           MOVE NEW-ACCTNO TO LTC-ACCTNO.
           PERFORM WRITE-LTC1-FILE.
           IF IO-FG NOT = 0
              PERFORM REWRITE-LTC1-FILE.

           MOVE HOLD-LT1-KEY TO LTC1-KEY.
           PERFORM READ-LTC1-FILE.
           IF IO-FG = 0
              PERFORM DELETE-LTC1-FILE.

       EXIT-RENUMBER-LTC.
           PERFORM CLOSE-LTC1-FILE.
     
      ***********************************
       RENUMBER-LTF-RECORDS SECTION.
      ***********************************
           PERFORM OPEN-LTF1-FILE.

           MOVE LTF-PATH-OWNBR  TO LTF-BRNO.
           MOVE LN-ACCTNO TO LTF-ACCTNO.
           MOVE 1         TO LTF-SEQNO.
           PERFORM READ-LTF1-FILE.
           IF IO-FG NOT = 0
              GO TO EXIT-RENUMBER-LTF.

           MOVE LTF1-KEY TO HOLD-LT1-KEY.

           MOVE NEW-ACCTNO TO LTF-ACCTNO.
           PERFORM WRITE-LTF1-FILE.
           IF IO-FG NOT = 0
              PERFORM REWRITE-LTF1-FILE.

           MOVE HOLD-LT1-KEY TO LTF1-KEY.
           PERFORM READ-LTF1-FILE.
           IF IO-FG = 0
              PERFORM DELETE-LTF1-FILE.


       EXIT-RENUMBER-LTF.
           PERFORM CLOSE-LTF1-FILE.
     
      ***********************************
       RENUMBER-LTI-RECORDS SECTION.
      ***********************************
           PERFORM OPEN-LTI1-FILE.

           MOVE LTI-PATH-OWNBR  TO LTI-BRNO.
           MOVE LN-ACCTNO TO LTI-ACCTNO.
           MOVE 1         TO LTI-SEQNO.
           PERFORM READ-LTI1-FILE.
           IF IO-FG NOT = 0
              GO TO EXIT-RENUMBER-LTI.

           MOVE LTI1-KEY TO HOLD-LT1-KEY.

           MOVE NEW-ACCTNO TO LTI-ACCTNO.
           PERFORM WRITE-LTI1-FILE.
           IF IO-FG NOT = 0
              PERFORM REWRITE-LTI1-FILE.

           MOVE HOLD-LT1-KEY TO LTI1-KEY.
           PERFORM READ-LTI1-FILE.
           IF IO-FG = 0
              PERFORM DELETE-LTI1-FILE.

       EXIT-RENUMBER-LTI.
           PERFORM CLOSE-LTI1-FILE.
     
      ***********************************
       RENUMBER-LTL-RECORDS SECTION.
      ***********************************
           PERFORM OPEN-LTL1-FILE.

           MOVE LTL-PATH-OWNBR  TO LTL-BRNO.
           MOVE LN-ACCTNO TO LTL-ACCTNO.
           MOVE 1         TO LTL-SEQNO.
           PERFORM READ-LTL1-FILE.
           IF IO-FG NOT = 0
              GO TO EXIT-RENUMBER-LTL.

           MOVE LTL1-KEY TO HOLD-LT1-KEY.

           MOVE NEW-ACCTNO TO LTL-ACCTNO.
           PERFORM WRITE-LTL1-FILE.
           IF IO-FG NOT = 0
              PERFORM REWRITE-LTL1-FILE.

           MOVE HOLD-LT1-KEY TO LTL1-KEY.
           PERFORM READ-LTL1-FILE.
           IF IO-FG = 0
              PERFORM DELETE-LTL1-FILE.

       EXIT-RENUMBER-LTL.
           PERFORM CLOSE-LTL1-FILE.
     
      ***********************************
       RENUMBER-LTP-RECORDS SECTION.
      ***********************************
           PERFORM OPEN-LTP1-FILE.

           MOVE LTP-PATH-OWNBR  TO LTP-BRNO.
           MOVE LN-ACCTNO TO LTP-ACCTNO.
           MOVE 1         TO LTP-SEQNO.
           PERFORM READ-LTP1-FILE.
           IF IO-FG NOT = 0
              GO TO EXIT-RENUMBER-LTP.

           MOVE LTP1-KEY TO HOLD-LT1-KEY.

           MOVE NEW-ACCTNO TO LTP-ACCTNO.
           PERFORM WRITE-LTP1-FILE.
           IF IO-FG NOT = 0
              PERFORM REWRITE-LTP1-FILE.

           MOVE HOLD-LT1-KEY TO LTP1-KEY.
           PERFORM READ-LTP1-FILE.
           IF IO-FG = 0
              PERFORM DELETE-LTP1-FILE.

       EXIT-RENUMBER-LTP.
           PERFORM CLOSE-LTP1-FILE.

      *********************************************
       FORM-RESET SECTION.
      *********************************************

           MOVE EXT-CONAME-CONAME TO NEWNUM-CONAME.
           MOVE EXT-CONAME-SYSDATE TO NEWNUM-SYSDATE.
           DISPLAY NEWNUM-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE NEWNUM-FORM-FIELDS TO WR-BUF.
           MOVE NEWNUM-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      *********************************************
       SETUP-LINK-INFO SECTION.
      *********************************************

           MOVE NEWNUM-HELP--FILE TO HELP-LINK-FILE.
           MOVE NEWNUM-HELP--DIR  TO HELP-LINK-DIR.

      *********************************************
       VDU-CONVERT-FIELD SECTION.
      *********************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBFX/NEWNUM_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ******************************************************************
      *
      *    C O P Y - L I B R A R Y - S E C T I O N S
      *
      ******************************************************************
       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/SCREEN.CPY".

      ************************************
       MISC-PARAGRAPHS SECTION.
      ************************************
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/GETPR.CPY".
       COPY "LIBGB/MKTEMP.CPY".
       COPY "LIBGB/SYSTEM.CPY".
       SEND-LEGEND.
           MOVE LEGEND TO WR-BUF.
           MOVE MESS-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      *************************
       IO-ROUTINE SECTION.
      *************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBSP/SPCJGS_SQL.CPY".
       COPY "LIBLP/LPNOGS_SQL.CPY".
       COPY "LIBLP/LPLXGGS_SQL.CPY".
       COPY "LIBLP/LPLXEGS_SQL.CPY".
       COPY "LIBLP/LPLXGS_SQL.CPY".
       COPY "LIBLP/LPLTPGS_SQL.CPY".
       COPY "LIBLP/LPLTLGS_SQL.CPY".
       COPY "LIBLP/LPLTIGS_SQL.CPY".
       COPY "LIBLP/LPLTFGS_SQL.CPY".
       COPY "LIBLP/LPLTCGS_SQL.CPY".
       COPY "LIBLP/LPLTBGS_SQL.CPY".
       COPY "LIBLP/LPLTAGS_SQL.CPY".
       COPY "LIBLP/LPLSXGS_SQL.CPY".
       COPY "LIBLP/LPLSRGS_SQL.CPY".
       COPY "LIBLP/LPLSPGS_SQL.CPY".
       COPY "LIBLP/LPLSMGS_SQL.CPY".
       COPY "LIBLP/LPLSLGS_SQL.CPY".
       COPY "LIBLP/LPLSJGS_SQL.CPY".
       COPY "LIBLP/LPLSGGS_SQL.CPY".
       COPY "LIBLP/LPLSFGS_SQL.CPY".
       COPY "LIBLP/LPLSDGS_SQL.CPY".
       COPY "LIBLP/LPLPGS_SQL.CPY".
       COPY "LIBLP/LPLNGS_SQL.CPY".
       COPY "LIBLP/LPLMGS_SQL.CPY".
       COPY "LIBLP/LPLHGS_SQL.CPY".
       COPY "LIBLP/LPLEGS_SQL.CPY".
       COPY "LIBLP/LPDTGS_SQL.CPY".
       COPY "LIBLP/LPCKGS_SQL.CPY".
       COPY "LIBLP/LPBWGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/GBPRINT.CPY".
       COPY "LIBSP/SPCJ1IN.CPY".
       COPY "LIBLP/LPBW1RN.CPY".
       COPY "LIBLP/LPCK1IN.CPY".
       COPY "LIBLP/LPNO1IN.CPY".
       COPY "LIBLP/LPDT1IN.CPY".
       COPY "LIBLP/LPLE1IN.CPY".
       COPY "LIBLP/LPLM1IN.CPY".
       COPY "LIBLP/LPLN1IN.CPY".
       COPY "LIBLP/LPLP1IN.CPY".
       COPY "LIBLP/LPLTA1IN.CPY".
       COPY "LIBLP/LPLTB1IN.CPY".
       COPY "LIBLP/LPLTC1IN.CPY".
       COPY "LIBLP/LPLTF1IN.CPY".
       COPY "LIBLP/LPLTI1IN.CPY".
       COPY "LIBLP/LPLTL1IN.CPY".
       COPY "LIBLP/LPLTP1IN.CPY".
       COPY "LIBLP/LPLX1IN.CPY".
       COPY "LIBLP/LPLXG1IN.CPY".
       COPY "LIBLP/LPLXE1IN.CPY".
       COPY "LIBLP/LPLH1IN.CPY".
       COPY "LIBLP/LPLSD1IN.CPY".
       COPY "LIBLP/LPLSF1IN.CPY".
       COPY "LIBLP/LPLSG1IN.CPY".
       COPY "LIBLP/LPLSJ1IN.CPY".
       COPY "LIBLP/LPLSL1IN.CPY".
       COPY "LIBLP/LPLSM1IN.CPY".
       COPY "LIBLP/LPLSP1IN.CPY".
       COPY "LIBLP/LPLSR1IN.CPY".
       COPY "LIBLP/LPLSX1IN.CPY".

       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO EXT-EOBUF-ERRCD.

      ***********************
       END-PROGRAM SECTION.
      ***********************
       END-RUN.
           PERFORM CLOSE-FILES.

           MOVE "C" TO ERRLOGW-ACTION.
           MOVE SPACES TO ERRLOGW-KEY.
           STRING BEG-BRANCH,",",OLD-ACCTNO,",",NEW-ACCTNO
                 DELIMITED BY "  " INTO ERRLOGW-KEY.
           MOVE FORM-PATHNAME TO ERRLOGW-PROGX.

           MOVE "CL/ERRLOG" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH ERRLOGW-AREA EXIT-PATHNAME.
           CANCEL FORM-PROGX.

       EXIT-PROG.

           IF ( EXT-ROUTE-BUF = "00" )
              MOVE "FX/FXMENU"  TO FORM-NAM
              MOVE FORM-PATH TO FORM-PATHNAME.

       STOP-RUN.
           IF EXT-ROUTE-BUF = "00" OR "40" OR "50"
              DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN
              DESTROY NEWNUM-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.
           EXIT PROGRAM.
