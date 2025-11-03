       IDENTIFICATION DIVISION.
       PROGRAM-ID.      VOIDI0.
       DATE-WRITTEN.    011585.
      ***********************************************************************
      *                 (LP)
      *  DESCRIPTION:   VOID CHECKS
      *                    3ONPI0 IN A15
      *
      *
      * LONPI0 = CASH RECEIPTS
      * DISBI0 = CASH DISBURSEMENTS
      * CHCKI0 = CHECK WRITING
      * VOIDI0 = VOID CHECKS
      * BNKDI0 = BANK DEPOSITS
      * BNKWI0 = BANK WITHDR
      * BCHKI0 = BATCH CHECK WRITING
      *
      * REV:
      *  060595 BAH REMOVED ALIGNMENT QUESTION, NOTHING EVER PRINTS
      *             FROM THIS PROGRAM !!
      *  062095 BLL WASN'T GETTING CK-DLRVNO TO TC-DLRVNO - CAUSED THIS
      *             PROGRAM TO GO TO $ & ALSO EMCKRG TO GO TO $
      *  MJD 062795 ADDED COPY MEMBERS LIBLP/POSTDATE AND POSTDATEW.
      *              SAVE TC-POSTTIME, POSTDATE, AND USERID.
      *  MJD 091595 ADDED DS-DATE-LOGIC.
      *  MJD 020896 ADDED UPDATE OF DEALER RESERVE FIELDS IN TY-REC
      *              (NET GAIN/LOSS)
      *  BLL 042996 IF DEALER RESERVE CK DLRV3, ADD BACK INTO AFP IN DS
      *  JTG 073096 ADDED LOGIC FOR DT-BATCHID-SEQ AND DT3-KEY
      *  JTG 103196 CHANGED TO USE NEW DT4-KEY (DT-CKNO), TO UNRELEASE
      *             THE BATCH
      *  JTG 011797 CORRECTED BUG, REVERSAL OF DT NOT LOOPINGING CORRECTLY
      *  JTG 012397 MADE REVERSE-DT RESET DT-RECORDS LIKE IT DID IN A65
      *  MJD 112597 EXPANDED CX-REC TO 426 BYTES
      *  MJD 032098 ADDED GETENV(W) FOR USE IN LIBLP/POSTDATE
      *  GLB 990413 DO NOT PERMIT VOIDING NOTES PAYABLE CHECK
      *  JTG 000324 MODIFIED TO UPDATE UNEARNED STATISTICS, MERCURY #223
      *             NO CHANGE REQUIRED
      *  GLB 000501 ASK IF REPLACEMENT CHECK WILL BE ISSUED. IF "YES"
      *      NET ACCOUNTING CHANGE = 0. OLD CHECK VOIDED, NEW MASTER
      *      CREATED. REPLACE CHECK# DT-RECORDS.
      * GLB 011011 TEST FOR CK-VOID = "R"(REPRINTED) AS WELL AS "Y"
      * BAH 020321 DO NOT READ LN AND BW AND DISPLAY THE MAKER INFO ON A
      *            LOAN CHECK, THE CHECK CAN BE ANY PAYEE !!!
      * BAH 020322 IF YOU ANSWER "N" TO ISSUE A REPLACEMENT CHECK, AND THE
      *            ACCOUNT HAS NOT BEEN VOIDED, ASK FOR PASSWORD
      *            LENDMARK, PR# 1733
      * GLB 030325 WORLD PR# 320;RECOMPILE DUE TO CHECKTRANSW/CHECKTRANS
      * GLB 030326 MOVE NEW FIELD CK-TILLNO TO CX-TILLNO WORLD PR# 320
      * BAH 030521 GOT CAUGHT WITH ONE SCREEN TRYING TO VOID THE CHECK,
      *            WHILE ANOTHER PERSON WENT AND VOIDED THE LOAN, WHICH
      *            VOIDED THE CHECK, THEN THE OTHER SCREEN VOIDED AGAIN
      *            CREATING ANOTHER "TC" RECORD. ADDED OPEN,READ,CLOSE OF CK
      *            TO CHECK CK-VOID RIGHT BEFORE THE UPDATE....REGMGM
      * GLB 030612 BUG TESTING CK-VOID FOR VOID UPDATE-ROUTINE; ONLY IF
      *            " "; ADDED "N" - BOTH INDICATING "NOT VOIDED"
      * BAH 031107 DO NOT ALLOW A CLEARED CHECK TO BE VOIDED...WORLD
      * BLV 040115 (BARB & BETH) ADDED OPEN-CX1 RIGHT BEFORE PERFORM OF
      *            UPDATE-FILES-CHECKTRANS & CLOSE RIGHT AFTER; BECAUSE
      *            OPEN & CLOSE OF CX TAKEN OUT OF COPY MEMBER CHECKTRANS
      *            BECAUSE CXFILE WASN'T STAYING LOCKED, CAUSING THIS:
      *            2 PEOPLE WERE ABLE TO PRINT THE SAME CHECK.
      *            1 WAS IN DOC PRINT, ENTER NEXT CHECK, WALKED AWAY TO
      *            ANOTHER SCREEN, WENT TO EXACT SAME PLACE, BUT ENTERED
      *            THE CHECK# AND SHOULD HAVE GOTTEN CX LOCKED, BUT DIDNT,
      *            IT ALLOWED IT TO PRINT, THEN WENT BACK TO OTHER TERMINAL
      *            AND WAS FORCED TO ENTER A CHECK # AGAIN, AND IT PRINTED
      *            AGAIN. SO, CX1 FILE NEEDS TO STAY LOCKED, DO NOT CLOSE
      * BLF 050322 AUTO NEXT CHECK NUMBER LOGIC WASN'T WORKING WITH RE-ISSUE
      *            CHECK OPTION, MULL PL# 470
      * BLF 060718 ADD GPENV & PAFILE FOR UPDATE OF PA-YTD-PAID WHEN CHECK
      *            IS VOIDED, DEVELOPMENT PR# 2633
      * CLS 060802 MOVE CK-EVENT-CD TO CX-EVENT-CD UNLESS IT'S "20"
      *            SECURITY PR# 2503
      * CLS 130913 WHEN USING F3 MULTIPLE CHECK OPTION IN SF INPUT,ALLOWS CHECK
      *            PAYABLE TO OTHERS (NOT DEALER).  WHEN YOU VOID A NON PAYABLE
      *            TO THE DEALER CHECK, IT SHOULD'T DISPLAY THE DEALER AS THE
      *            PAYEE, GET IT FROM THE CK RECORD. IF YOU CHOSE TO RE-ISSUE
      *            THE CHECK, IT SET THE PAYEE TO THE DEALER EVEN THOUGH THAT
      *            WASNT THE PAYEE FROM THE CHECK RECORD. I CHANGED DISPLAY-
      *            DEALER MASTER SECTION TO DISPLAY THE MASTER IF CX-PAYEE =
      *            DL-NAME, OTHERWISE USE CX-PAYEE  CFSC PR#813
      * BAH 131111 WHEN VOIDING A "DD" DRAWCK, MAKE SURE CASH DRAWER IS
      *            OPEN OR DONT ALLOW IT, WORLD PR# 943
      * BAH 140502 RENAMED FROM 3ONPI0, VERYANT PROGRAMS CANT START NUMERIC
      * BAH 140814 ADDED TEST FOR LPSMNU IN HELP
      *  CS 160201 REMOVED GIFILE, NOT USED
      *            SAVED VERSION VOIDI0.160201
      * BAH 160304 CHANGED TILLNO TO NUMERIC
      * BLM 160513 REMOVE LPSETDSW, LPSETDS, PD0003
      * BAH 160908 MEXICO CHECK REPRINT WAS NOT BUILDING LONG NAMES LIKE THE
      *            ORIGINAL CHECK PRINT DOES, SO ASK A QUESTION IF THIS
      *            REPRINT IS A F3 CHECK TO BORROWER AND SET CX-BWPROC SO 
      *            CHECK PRINT PROGRAM (LONP0Z) WILL BUILD NAME, WORLD 1186
      * BAH 160921 PASSWORD DISPLAYING EXISTING CHECK INFO, COMM LENDER 5174
      * BAH 170309 REPRINT SPANISH CHECK ASKS IF CHECK TO BORROWER TO GET
      *            LONG NAMES, BUT WAS NOT MOVING VDUBUF TO ACTION SO IT
      *            DID NOT FUNCTION CORRECTLY, WORLD PR#1232
      *  CS 170612 ACTIVATE 8 DIGIT DATES
      * BAH 181115 GLOBAL FMFILE
      * BAH 20190613 ADDED CLEAR-FM-FILE (LPFMCL) AND SET FM-USERID
      * BAH 2022.0314 REMOVED EXT-SPANISH-FORMS AND ALL MX FORMS
      * BAH 20230908 REMOVED PA-FILE
      ***********************************************************************
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
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)LP/VOIDI0.CBL S35 02/20/24-14:35:30 >".
      **********************************
      *       F I L E   I F N          *
      **********************************

       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBGB/GB01GB.CPY".
       COPY "LIBGB/GB01GB_SQL.CPY".
       COPY "LIBGB/GBWSGB.CPY".
       COPY "LIBLP/LP01TRC.CPY".
       COPY "LIBLP/LP01TRC_SQL.CPY".
       COPY "LIBLP/LPWSTRC.CPY".
       COPY "LIBLP/LP01CA.CPY".
       COPY "LIBLP/LP01CA_SQL.CPY".
       COPY "LIBLP/LPWSCA.CPY".
       COPY "LIBLP/LP01CK.CPY".
       COPY "LIBLP/LP01CK_SQL.CPY".
       COPY "LIBLP/LPWSCK.CPY".
       COPY "LIBLP/LP01CX.CPY".
       COPY "LIBLP/LP01CX_SQL.CPY".
       COPY "LIBLP/LPWSCX.CPY".
       COPY "LIBLP/LP01DL.CPY".
       COPY "LIBLP/LP01DL_SQL.CPY".
       COPY "LIBLP/LPWSDL.CPY".
       COPY "LIBLP/LP01DT.CPY".
       COPY "LIBLP/LP01DT_SQL.CPY".
       COPY "LIBLP/LPWSDT.CPY".
       COPY "LIBLP/LP01DS.CPY".
       COPY "LIBLP/LP01DS_SQL.CPY".
       COPY "LIBLP/LPWSDS.CPY".
       COPY "LIBLP/LP01FD.CPY".
       COPY "LIBLP/LP01FD_SQL.CPY".
       COPY "LIBLP/LPWSFD.CPY".
       COPY "LIBLP/LP01FM.CPY".
       COPY "LIBLP/LP01FM_SQL.CPY".
       COPY "LIBLP/LPWSFM.CPY".
       COPY "LIBLP/LP01LN.CPY".
       COPY "LIBLP/LP01LN_SQL.CPY".
       COPY "LIBLP/LPWSLN.CPY".
       COPY "LIBLP/LP01TY.CPY".
       COPY "LIBLP/LP01TY_SQL.CPY".
       COPY "LIBLP/LPWSTY.CPY".
       COPY "LIBLP/CHECKTRANSW.CPY".
       01  REPLACEMENT-CHECK-TYPE  PIC X VALUE "M".
       01  DISPLAY-CHECK-NO        PIC Z(6).

       COPY "LIBLP/LPIPBUF.CPY".
       COPY "LIBGB/CAPASSWDW.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBGB/PASSWDW.CPY".
       78  REPRINT-SEQ             VALUE 0.
       78  REPRINT-NO-SEQ          VALUE 1.
       78  DISPLAY-SEQ             VALUE 2.

       COPY "LIBGB/GPENVW.CPY".
       COPY "LIBGB/SYSTEMW.CPY".

       COPY "LIBGL/ALLENTW.CPY".

       COPY "LIBLP/LPDSCLW.CPY".
       COPY "LIBLP/LPMENUW.CPY".
       COPY "LIBLP/LPSETTYW.CPY".
       COPY "LIBLP/POSTDATEW.CPY".
       COPY "LIBWI/CKSCANW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBLP/VOIDI_DEF.CPY".
       COPY "LIBLP/LPSMNU_DEF.CPY".
       COPY "LIBLP/LPLNMU_DEF.CPY".
       COPY "LIBLP/LPCAMU_DEF.CPY".
       COPY "LIBLP/LPVDMU_DEF.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBLP/VOIDI_WKS.CPY".
       COPY "LIBLP/LPSMNU_WKS.CPY".
       COPY "LIBLP/LPLNMU_WKS.CPY".
       COPY "LIBLP/LPCAMU_WKS.CPY".
       COPY "LIBLP/LPVDMU_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

       COPY "LIBGB/GETFMW.CPY".
       COPY "LIBLP/LPLOANW.CPY".
       COPY "LIBLP/LPPFBUF.CPY".

       SCREEN SECTION.
       COPY "LIBLP/VOIDI_SCN.CPY".
       COPY "LIBLP/LPSMNU_SCN.CPY".
       COPY "LIBLP/LPLNMU_SCN.CPY".
       COPY "LIBLP/LPCAMU_SCN.CPY".
       COPY "LIBLP/LPVDMU_SCN.CPY".

       PROCEDURE DIVISION
            USING FORM-PATHNAME EXIT-PATHNAME PROG-BUF UPDATE-BUF.
       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
               PR-FILE.
       COPY "LIBGB/DECLARE.CPY".
           PERFORM CLOSE-TY1-FILE.
           PERFORM CLOSE-TRC1-FILE.
           PERFORM CLOSE-LN1-FILE.
           PERFORM CLOSE-FM1-FILE.
           PERFORM CLOSE-FD1-FILE.
           PERFORM CLOSE-DT4-FILE.
           PERFORM CLOSE-DT2-FILE.
           PERFORM CLOSE-DT1-FILE.
           PERFORM CLOSE-DS1-FILE.
           PERFORM CLOSE-DL1-FILE.
           PERFORM CLOSE-CX1-FILE.
           PERFORM CLOSE-CK1-FILE.
           PERFORM CLOSE-CA1-FILE.
           PERFORM CLOSE-GB-FILE.
           PERFORM CLOSE-BR-FILE.
           PERFORM CLOSE-PR-FILE.
       COPY "LIBGB/DECLAR2.CPY".

      ***************************************
       MAIN-PROGRAM SECTION.
      ***************************************

           PERFORM SQL-CONNECT.

      D    STOP MESS.
           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "VOIDI" TO VDU-FORM-NAME.

           PERFORM FORM-RESET.


           PERFORM GET-GPENV.

           PERFORM GET-GLOBAL.

           PERFORM GET-BRANCH.

           MOVE 0 TO Q-POS C-POS.
           MOVE VOIDI-PRU-POS    TO P-POS.
           MOVE "N" TO Q-BUF.
           MOVE "01" TO C-BUF.

           MOVE "S  A020PRU." TO VDU.
           MOVE 00 TO VDUBUF IP-PRU.
           PERFORM SCREENIO.

      * INITIALIZED IN CASE OF EXIT BEFORE FD-FILE
      * KEY IS SET
           MOVE 0 TO FD-FMNO.
           MOVE 0 TO IP-FMNO.

       SETRP.
           MOVE "F7-MENU, ENTER PRINTER NUMBER:" TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "N" TO STAT.
           PERFORM PRU-IN.
           IF STAT = "1>"
              PERFORM PRU-OUT
              DESTROY ALL CONTROLS
              PERFORM MENU-SELECTION
              IF MENU-FG = 99
                 GO TO SETRP
              ELSE
                 MOVE 0 TO LOAN-KEY
                 MOVE "R" TO ROUTE-CD
                 GO TO END-PROG.
           IF STAT NOT = "00"
              GO TO SETRP.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    PRINTER ALREADY RESERVED AT THIS POINT.
      *    IF A NEW PROGRAM IS SELECTED VIA THE MENU,
      *    MOVE "R" TO ROUTE-CD TO REMOVE LFN 26.
      *    ROUTE-CD REMAINS " " FOR PRINTING.
      *    UPON EXITING THIS PROGRAM ERRCD SHOULD
      *    ALWAYS BE "S" (TO USE SPECIAL TCL SUB-ROUTINES).
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

      * NEED TO INITIALIZE WHAT THESE FIELDS WERE IN THE A15 FORM THAT
      * THE FLDVAL WOULD HAVE SUCKED IN;

           MOVE SPACES TO IP-PROG BATCH-PR.
           MOVE "N" TO IP-AOK QUEUE-PRINT.
           MOVE "Y" TO CONT-PRINT.
           MOVE "P" TO IP-PRFG.

      * IF UP ARROW AFTER ALIGNMENT; IP-FMNO = "  5" & GIVES READ ERROR
           IF IP-FMNO NOT NUMERIC
              MOVE 5 TO IP-FMNO.

       RETURN-FROM-PRINT.
           MOVE " " TO ROUTE-CD.
           MOVE "S" TO ERRCD.
           IF IP-AOK = "R"
      * RETURNING FROM PRINT PROGRAM
              PERFORM RESET-SCREEN
              PERFORM SEND-PRU-FORM
              MOVE "Y" TO IP-AOK.
      * TEST THAT FORM WAS NOT ALIGNED
           IF IP-AOK NOT = "Y"
              GO TO TEST-FMNO.
      * RECHECK ALIGNMENT
           PERFORM GET-FD1.
           IF IO-FG = 9
              GO TO IO-ERROR.

           GO TO FORM-ALIGNED-YN.

      *********************************
      *   P R I N T   O P T I O N S   *
      *********************************
       ENTER-PROG.
      * TO OVERRIDE M2 AS DEFAULT CHECK PROG (CTL P FROM FMNO)
           MOVE "ENTER PROGRAM ID OF PRINT PROGRAM:" TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "E RA020IP." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS NOT = "00"
              GO TO ENTER-PROG.
           MOVE VDUBUF TO PRINT-PROG.
       ENTER-FMNO.
           MOVE "F7-MENU, ^-PRU, ENTER FORM NUMBER:" TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "FMX." TO MESS.
           PERFORM RESET-MESS.
           MOVE "EN N030FM." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>"
              PERFORM PRU-OUT
              DESTROY ALL CONTROLS
              PERFORM MENU-SELECTION
              IF MENU-FG = 99
                 GO TO ENTER-FMNO
              ELSE
                 MOVE 0 TO LOAN-KEY
                 MOVE "R" TO ROUTE-CD
                 GO TO END-PROG.
           IF VDUSTATUS = "1U"
              GO TO SETRP.
           IF VDUSTATUS = "1P"
              GO TO ENTER-PROG.
           IF VDUSTATUS NOT = "00"
              GO TO ENTER-FMNO.
           MOVE VDUNUM0 TO IP-FMNO.
       TEST-FMNO.
           MOVE "SN N030FM." TO VDU.
           MOVE 5 TO IP-FMNO VDUNUM0.
           PERFORM SCREENIO.
       GET-FMNO.
           PERFORM GET-FD1.
           IF IO-FG = 9
              PERFORM NOT-FOUND
              GO TO ENTER-FMNO.
           MOVE "S  A000FMX." TO VDU.
           MOVE FD-DESC TO VDUBUF.
           PERFORM SCREENIO.
           IF FD-PROG NOT = GB-CHECK-PROG
              MOVE "WRONG FORM" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-FMNO.

       FORM-ALIGNED-YN.
           MOVE FD-PROG TO IP-PROG.
           IF PRINT-PROG NOT = " "
              MOVE PRINT-PROG TO IP-PROG.
           MOVE "Y" TO IP-AOK.
           PERFORM RESET-SEL.

      *********************************
      *   C H E C K   E N T R Y       *
      *********************************
       MAIN-MODULE.
           MOVE SPACES TO DISPLAY-BUF DISPLAY-HEAD-BUF.
           IF FIRST-TIME = 0
              MOVE 1 TO FIRST-TIME.
           PERFORM RESET-SCREEN.

           PERFORM ENTER-KEYS.
           IF IO-TYPE = "PRU"
              GO TO ENTER-FMNO.
           IF IO-TYPE = "CLR"
              GO TO MAIN-MODULE.

      *---------------------------------------------------------
      * CALL PRINT PROGRAM: LONPM2, LONP2M, ETC.
      *---------------------------------------------------------
           IF IO-TYPE = "END"
              MOVE IP-PROG TO PROG-WK-IP
              MOVE PROG-WK TO FORM-NAM
              PERFORM XFER-FM-FIELDS
              CALL FORM-PROGX USING FORM-PATH PROG-BUF IP-BUF PRU-PATH
              CANCEL FORM-PROGX
              GO TO RETURN-FROM-PRINT.

           GO TO MAIN-MODULE.

      ************************************
       ENTER-KEYS SECTION.
      ************************************
       RESEND-PRU-FMNO.

      * PRU AND FMNO NOT REENTERED UPON F1 RESET.
           PERFORM SEND-PRU-FORM.

      *---------------------------------------------------------
      * ENTER TRANS TYPE; S=SYSTEM,   M=MANUAL
      *                  *V=VOID,     R=RSV REC
      *                   C=CASH REC, B=BANK           * = VALID
      *---------------------------------------------------------
       ENTER-TRANS-TYPE.
           MOVE "N" TO CXFILE-SCAN-FLAG.
           MOVE 0 TO NEW-FLAG CHECK-NO.
           MOVE SPACES TO IO-TYPE.

           PERFORM NEW-RECORD-SETUP.

      * FORCE "V"OID
           MOVE "V" TO TRANS-TYPE CX-PAID.
           MOVE CX-PAID          TO HOLD-TRANS-TYPE.

           PERFORM DISPLAY-SCREEN-HEADER.

       ENTER-CHECK-NO.
           MOVE "F7-MENU, ^-FORM #, ENTER CHECK NO. TO VOID:" TO MESS.
           PERFORM SEND-LEGEND.

           MOVE "EN N060K030." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO EXIT-ENTER-KEYS.
           IF VDUSTATUS = "1>"
              GO TO MENU-KEY.
           IF VDUSTATUS = "1U"
              MOVE "PRU" TO IO-TYPE
              GO TO EXIT-ENTER-KEYS.
           IF VDUSTATUS = "1<"
              PERFORM CKFILE-SCAN
              IF IO-FG = 9
                 GO TO ENTER-CHECK-NO
              ELSE
                 GO TO FIND-CHECK-NO.
           IF VDUSTATUS NOT = "00"
              GO TO ENTER-CHECK-NO.

           INSPECT VDUNUM0 REPLACING ALL " " BY "0".
           IF VDUNUM0 = 0
              GO TO ENTER-CHECK-NO.

           MOVE VDUNUM0 TO CK-NO CHECK-NO CX-CKNO.
           MOVE BRANCH TO CK-BRNO CX-BRNO.
       FIND-CHECK-NO.
           PERFORM OPEN-CK1-FILE.
           PERFORM READ-CK1-FILE.
           PERFORM CLOSE-CK1-FILE.
           IF IO-FG = 9
              PERFORM NOT-FOUND
              PERFORM REFRESH-SCREEN
              GO TO ENTER-CHECK-NO.

           PERFORM DISPLAY-CK-FIELDS.

           IF CK-CLEARED-DATE NOT = 0
              MOVE "CHECK CLEARED BANK, VOID NOT ALLOWED" TO MESS
              PERFORM SEND-MESS
              MOVE "CLR" TO IO-TYPE
              GO TO EXIT-ENTER-KEYS.
           IF CK-VOID = "Y"
              MOVE "CHECK PREVIOUSLY VOIDED" TO MESS
              PERFORM SEND-MESS
              MOVE "CLR" TO IO-TYPE
              GO TO EXIT-ENTER-KEYS.
           IF CK-VOID = "R"
              MOVE "CHECK PREVIOUSLY VOIDED & REISSUED" TO MESS
              PERFORM SEND-MESS
              MOVE "CLR" TO IO-TYPE
              GO TO EXIT-ENTER-KEYS.

      *  MAKE SURE DRAWER IS OPEN BEFORE ALLOWING VOID
           IF CK-TYPE = "DD"
              PERFORM OPEN-CA1-FILE
              MOVE CK-BRNO    TO CA-BRNO
              MOVE TRANS-DATE TO CA-DATE
              MOVE CK-TILLNO  TO CA-TILLNO
              PERFORM READ-CA1-FILE
              PERFORM CLOSE-CA1-FILE
              IF IO-FG = 9 OR CA-CLOSED-HHMM NOT = 0
                 MOVE "CASH DRAWER NOT OPEN" TO MESS
                 PERFORM SEND-MESS
                 MOVE "CLR" TO IO-TYPE
                 GO TO EXIT-ENTER-KEYS.

           IF CK-TYPE = "N"
              MOVE "VOIDING NOTES PAYABLE CHECK RESTRICTED !" TO MESS
              PERFORM SEND-MESS
              MOVE "REVERSE LINE IN TRANSACTION POSTING" TO MESS
              PERFORM SEND-MESS
              MOVE "CLR" TO IO-TYPE
              GO TO EXIT-ENTER-KEYS.

           PERFORM SETUP-CX-FROM-CK.

           IF (DEALER)
              IF CK-DLNO = 202020
      * ENTER DEALER NO. IF OLD RECORDS
                 PERFORM PROCESS-OLD-RECORDS
                 IF IO-TYPE = "CLR"
                    GO TO EXIT-ENTER-KEYS.
           IF (DEALER)
              PERFORM DISPLAY-DEALER-MASTER.

       CONT-VOID.
           MOVE "ISSUE A REPLACEMENT CHECK ? (Y/N)" TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "R RA060SEL." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO EXIT-ENTER-KEYS.
           IF VDUSTATUS NOT = "00"
              GO TO CONT-VOID.
           MOVE VDUBUF TO ACTION.
           IF ACTION = "     N"
              MOVE 0 TO CX-CKNO-NEW
              PERFORM CHECK-VOID-LOAN
              IF PASSWDW-STAT = "OK"
                 GO TO CONFIRM-VOID
              ELSE
                 MOVE "MUST VOID ACCOUNT FIRST TO DECLINE REISSUE"
                       TO MESS
                 PERFORM SEND-MESS
                 GO TO CONT-VOID.

           IF ACTION NOT = "     Y"
              GO TO CONT-VOID.

      * TEST PASSWORD IF ISSUING A NEW CHECK
           MOVE REPRINT-SEQ             TO PASSWDW-SEQ.
           MOVE "TO ISSUE A NEW CHECK " TO PASSWDW-PROMPT2.
           PERFORM VERIFY-PASSWORD.
           IF PASSWDW-STAT = "OK"
              GO TO ENTER-CHECK-NO2.

           MOVE "NOT AUTHORIZED TO REPRINT CHECKS !" TO MESS.
           PERFORM SEND-MESS.
           GO TO CONT-VOID.

      * ENTER NEW CHECK NO.
      
       ENTER-CHECK-NO2.

           IF BR-AUTO-CHECK NOT = "Y"
              GO TO CONTINUE-ENTER-CHECK2.

           MOVE "REPLACEMENT CHECK TYPE (M=MANUAL, S=SYSTEM)?" TO MESS.
           PERFORM SEND-LEGEND.

           MOVE "E RA060SEL." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS NOT = "00"
              GO TO ENTER-CHECK-NO2.
           IF VDUBUF NOT = "     M" AND
                      NOT = "     S"
              GO TO ENTER-CHECK-NO2.

      * MUST READ BRANCH AGAIN, BECAUSE WHEN YOU COME BACK FROM
      * PRINTING THE FIRST ONE, YOU NEED THE UPDATED NUMBER
           IF VDUBUF = "     M"
              MOVE "M" TO REPLACEMENT-CHECK-TYPE CX-CKPAID-NEW
              GO TO CONTINUE-ENTER-CHECK2
           ELSE
              MOVE "S" TO REPLACEMENT-CHECK-TYPE CX-CKPAID-NEW
              PERFORM GET-BRANCH
              MOVE "SN N060CKNO." TO VDU
              MOVE BR-NEXT-CHECK TO VDUNUM0
              PERFORM SCREENIO
              GO TO TEST-CHECK-NO2.

       CONTINUE-ENTER-CHECK2.
           MOVE "ENTER REPLACEMENT CHECK # :" TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "EN N060CKNO." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO EXIT-ENTER-KEYS.
           IF VDUSTATUS = "1>"
              GO TO MENU-KEY.
           IF VDUSTATUS = "1U"
              MOVE "NEWK030." TO MESS
              PERFORM RESET-MESS
              GO TO ENTER-CHECK-NO.
           IF VDUSTATUS NOT = "00"
              GO TO ENTER-CHECK-NO2.

           INSPECT VDUNUM0 REPLACING ALL " " BY "0".
           IF VDUNUM0 = 0
              GO TO ENTER-CHECK-NO2.

       TEST-CHECK-NO2.
           MOVE VDUNUM0 TO CK-NO CX-CKNO-NEW.
           MOVE CX-BRNO TO CK-BRNO.
           PERFORM OPEN-CK1-FILE.
           PERFORM READ-CK1-FILE.
           PERFORM CLOSE-CK1-FILE.
           IF IO-FG = 0
              IF BR-AUTO-CHECK NOT = "Y"
                 MOVE "CHECK ALREADY EXISTS, ENTER A NEW CHECK NO.!"
                    TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-CHECK-NO2
              ELSE
                 MOVE "PLEASE CORRECT NEXT CHECK NUMBER" TO MESS
                 PERFORM SEND-MESS
                 PERFORM REFRESH-SCREEN
                 GO TO ENTER-CHECK-NO2.

      * MANUAL CHECKS DURING AUTO CHECK NUMBERING MUST HAVE A NUMBER
      * LESS THAN THE NEXT BRANCH CHECK NUMBER. THEY MUST RIP THE CHECK
      * OFF PRINTER, ADVANCE THE NEXT-CHECK NUMBER IN BR-REC, THEN THEY
      * INPUT THE CORRECT NUMBER, BECAUSE IT WILL BE 1 NUMBER LESS.
           IF REPLACEMENT-CHECK-TYPE = "M"
              IF BR-AUTO-CHECK = "Y"
                 IF CK-NO NOT < BR-NEXT-CHECK
                    MOVE "MANUAL CHECK NUMBER MUST BE < $$$$$$"
                          TO MESS
                    MOVE BR-NEXT-CHECK TO DISPLAY-CHECK-NO
                    INSPECT MESS REPLACING FIRST "$$$$$$"
                          BY DISPLAY-CHECK-NO
                    PERFORM SEND-MESS
                    GO TO ENTER-CHECK-NO2.

       CONFIRM-REISSUE.
           MOVE CX-CKNO TO CHECK-NO.
           MOVE "VOID # &&&&&&; ISSUE # $$$$$$" TO CX-REASON.
           INSPECT CX-REASON REPLACING FIRST "&&&&&&" BY CHECK-NO.
           MOVE CX-CKNO-NEW TO CHECK-NO.
           INSPECT CX-REASON REPLACING FIRST "$$$$$$" BY CHECK-NO.
           MOVE SPACES TO MESS.
           STRING "F1-CANCEL, " CX-REASON " ? (Y/N)" DELIMITED BY SIZE
              INTO MESS.
           PERFORM SEND-LEGEND.

           MOVE CX-CKNO TO CHECK-NO.

           MOVE "R RA060SEL." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO EXIT-ENTER-KEYS.
           IF VDUSTATUS NOT = "00"
              GO TO CONFIRM-REISSUE.
           MOVE VDUBUF TO ACTION.
           IF ACTION = "     N"
              MOVE "CLR" TO IO-TYPE
              GO TO EXIT-ENTER-KEYS.
           IF ACTION NOT = "     Y"
              GO TO CONFIRM-REISSUE.

           GO TO PROCESS-VOID.

       CONFIRM-VOID.
           MOVE CX-CKNO TO CHECK-NO.
           MOVE "F1-CANCEL,  VOID  CHECK# &&&&&&  (Y/N) " TO MESS.
           INSPECT MESS REPLACING FIRST "&&&&&&" BY CHECK-NO.
           PERFORM SEND-LEGEND.
           MOVE "R RA060SEL." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO EXIT-ENTER-KEYS.
           IF VDUSTATUS NOT = "00"
              GO TO CONFIRM-VOID.

           MOVE VDUBUF TO ACTION.
           IF ACTION = "     N"
              MOVE "CLR" TO IO-TYPE
              GO TO EXIT-ENTER-KEYS.
           IF ACTION NOT = "     Y"
              GO TO CONFIRM-VOID.

      **********************************************************
      *   V O I D    C H E C K    R O U T I N E S
      **********************************************************

       PROCESS-VOID.
             
           MOVE CX-BRNO TO CK-BRNO.
           MOVE CX-CKNO TO CK-NO.
           PERFORM OPEN-CK1-FILE.
           PERFORM READ-CK1-FILE.
           IF IO-FG = 9
              GO TO IO-ERROR.
           PERFORM CLOSE-CK1-FILE.

      * GOT CAUGHT WITH ONE SCREEN TRYING TO VOID THE CHECK, WHILE ANOTHER
      * PERSON WENT AND VOIDED THE LOAN, WHICH VOIDED THE CHECK, THEN
      * THE OTHER SCREEN VOIDED AGAIN CREATING ANOTHER "TC" RECORD.
      * GLB  06/12/03 PREVIOUSLY ONLY TESTING " "
           IF CK-VOID = " " OR "N"
              PERFORM UPDATE-ROUTINE.

           GO TO EXIT-ENTER-KEYS.

      *******************************************************
      *   E X I T      P R O G R A M
      *******************************************************

       MENU-KEY.
           PERFORM PRU-OUT.

           DESTROY ALL CONTROLS.

           PERFORM MENU-SELECTION.
           IF MENU-FG = 99
              GO TO ENTER-CHECK-NO
           ELSE
              MOVE 0 TO LOAN-KEY
              MOVE "R" TO ROUTE-CD
              MOVE "END" TO IO-TYPE
              GO TO END-PROG.

       EXIT-ENTER-KEYS.
           EXIT.

      *********************************
      *   V E R I F Y   T Y P E       *
      *   VERIFY TYPE CODE;           *
      *   1 =  MANUAL CHECK
      *   2 =  SYSTEM CHECK
      *   3 =  VOID   CHECK           *
      *   4 =  DEALER RSV CASH REC    *
      *   5 =  CASH RECEIPT           *
      *   6 =  BANK TRANSACTION       *
      *   DISPLAY TYPE DESCRIPTION;   *
      *   SET TYPE SUBSCRIPT.         *
      *********************************
       VERIFY-TRANS-TYPE SECTION.
           MOVE 0 TO TYPE-SUB.
           MOVE SPACES TO VDUBUF TRANS-DESC.
       NEXT-TYPE-CHECK.
           ADD 1 TO TYPE-SUB.
           IF T-CODE(TYPE-SUB) = TRANS-TYPE
              MOVE T-DESC(TYPE-SUB) TO VDUBUF TRANS-DESC
              GO TO EXIT-VERIFY-TYPE.
           IF TYPE-SUB < TYPE-MAX
              GO TO NEXT-TYPE-CHECK.
           COMPUTE TYPE-SUB = TYPE-MAX + 1.
       EXIT-VERIFY-TYPE.
           EXIT.

      *****************************************************
      *    VERIFY PAYEE
      *      -  VERIFY PAYEE CODE
      *      -  DISPLAY PAYEE DESCRIPTION
      *      -  GET TT-CKWRITTEN SUBSCRIPT
      *      -  LOCK CHECK ENTRY FIELDS
      *********************************
       VERIFY-CHECK-PAYEE SECTION.
           MOVE 0 TO PAYEE-SUB.
       NEXT-PAYEE-VERIFY.
           ADD 1 TO PAYEE-SUB.
           IF P-CODE(PAYEE-SUB) = " "
              GO TO NEXT-PAYEE-VERIFY.
           IF P-CODE(PAYEE-SUB) = CHECK-PAYEE
              GO TO FOUND-PAYEE.
           IF PAYEE-SUB < PAYEE-MAX
              GO TO NEXT-PAYEE-VERIFY.
           ADD 1 TO PAYEE-MAX GIVING PAYEE-SUB.
           GO TO EXIT-VERIFY-PAYEE.
       FOUND-PAYEE.
           MOVE P-CODE(PAYEE-SUB) TO VDUBUF PAYEE-CODE.
           MOVE P-DESC(PAYEE-SUB) TO VDUBUF PAYEE-DESC.

           MOVE CHFG-PTR(TYPE-SUB, PAYEE-SUB) TO SUB.
           MOVE CHFG-VAL(SUB) TO CHFG-TABLE.

           IF (LOANCK)
              IF CXFILE-SCAN-FLAG NOT = "Y"
              MOVE DIRECT-NOSCAN TO CHFG-TABLE.
           IF (BANKTR)
              IF CX-DEPOSIT-WITHDR = "D"
                 MOVE "BANK DEPOSIT" TO PAYEE-DESC.

       EXIT-VERIFY-PAYEE.
           MOVE "S  A000P010." TO VDU.
           IF (LOANCK)
              MOVE "S.S.#" TO VDUBUF.
           IF (DEALER)
              MOVE "DEALER" TO VDUBUF.
           IF (BANKTR)
              MOVE "--------" TO VDUBUF.
           IF (MISCCK)
              MOVE " " TO VDUBUF.
           PERFORM SCREENIO.

      *********************************
       CHECK-GL-BALANCE SECTION.
      *********************************
           MOVE 0 TO BAL-FG VDUNUM2.
           IF CXFILE-SCAN-FLAG = "Y"
                 MOVE CX-AMT TO VDUNUM2.
      * TESTING DEALER CHECK OR DEALER RESERVE CASH RECEIPTS (SEE LONPI0)
           IF (DEALER)
              OR TRANS-TYPE = "R"
                 MOVE CX-AMT TO VDUNUM2.

      * NOTE FROM LONPI0: CASH RECEIPTS, TRANS TYPE "R"
      *    ENTERING A POSITIVE NUMBER IN CX-AMT NOW !!
      *    RESERVES DO NOT REQUIRE ANY GLNO, KNOWS ITS RESERVES
       NEXT-GLAMT-SUM.
           ADD 1 TO BAL-FG.
           ADD CX-GLAMT(BAL-FG) TO VDUNUM2.
           IF BAL-FG < 5
              GO TO NEXT-GLAMT-SUM.
           IF CX-AMT NOT = VDUNUM2
              MOVE 9 TO BAL-FG.

      **********************************
       CHECK-VOID-LOAN SECTION.
      **********************************

      * NEED THIS TO EXIT OK IF ACCOUNT IS VOIDED OR NO LOAN (MS)
           MOVE "OK" TO PASSWDW-STAT.

           MOVE CK-ACCTNO TO LN-ACCTNO
           PERFORM OPEN-LN1-FILE.
           PERFORM READ-LN1-FILE.
           PERFORM CLOSE-LN1-FILE.
           IF IO-FG = 9
              GO TO EXIT-CHECK-VOID-LOAN.

      * IF ACCOUNT HAS NOT BEEN VOIDED, ASK FOR A PASSWORD TO NOT
      * ISSUE A REPLACEMENT, THEN THEY MUST GO VOID THE ACCOUNT THEN
      * THEY CAN COME BACK HERE AND ANSWER NO TO REPLACEMENT WITHOUT
      * A PASSWORD, LENDMARK
           IF LN-POCD NOT = "**"
              MOVE REPRINT-NO-SEQ TO PASSWDW-SEQ
              MOVE "TO DECLINE A REISSUE " TO PASSWDW-PROMPT2
              PERFORM VERIFY-PASSWORD.

      * WILL EXIT WITH WHATEVER RESULTS FROM PASSWORD
       EXIT-CHECK-VOID-LOAN.
           EXIT.

      *********************************
      * U P D A T E   R O U T I N E S *
      *********************************
      *   1 =  MANUAL CHECK           *
      *   2 =  SYSTEM CHECK           *
      *   3 =  VOID   CHECK           *
      *   4 =  DEALER RSV CASH REC    *
      *   5 =  CASH RECEIPT           *
      *   6 =  BANK TRANSACTION       *
      *********************************
       UPDATE-ROUTINE SECTION.
           MOVE HOLD-TRANS-TYPE  TO CX-PAID TRANS-TYPE.
           PERFORM VERIFY-TRANS-TYPE.

           GO TO
              UPDATE-MANUAL UPDATE-SYSTEM
              UPDATE-VOID   UPDATE-CASH
              UPDATE-RSVREC UPDATE-BANK
                            DEPENDING ON TYPE-SUB.

      * SEE VERIFY-TRANS-TYPE FOR DETAILS.
           MOVE "INVALID CHECK TYPE" TO MESS.
           PERFORM SEND-MESS.

           MOVE "E" TO ERRCD.

           GO TO END-ROUTINE.

      *******************************************
      *        N E W    C H E C K S
      *******************************************

       UPDATE-MANUAL.
           MOVE "UPDATE-MANUAL CHECK NOT VALID !" TO MESS.
           PERFORM SEND-MESS.
           MOVE "CLR" TO IO-TYPE.
           GO TO EXIT-UPDATE-ROUTINE.

       UPDATE-SYSTEM.
           MOVE "UPDATE-SYSTEM CHECK NOT VALID !" TO MESS.
           PERFORM SEND-MESS.
           MOVE "CLR" TO IO-TYPE.
           GO TO EXIT-UPDATE-ROUTINE.

      ******************************************
      *        V  O  I  D    C  H  E  C  K  S
      ******************************************

       UPDATE-VOID.
           IF CX-CKNO-NEW = 0
               GO TO SKIP-PRINT-NEW-CHECK.

           IF BR-AUTO-CHECK = "Y"
              IF REPLACEMENT-CHECK-TYPE = "M"
                 GO TO SKIP-PRINT-NEW-CHECK
              ELSE
                 GO TO PRINT-REPLACEMENT-CHECK.

           MOVE "PRINT NEW CHECK (Y/N/) ?       " TO MESS.
           PERFORM SEND-LEGEND.

           MOVE "E RA060SEL." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS NOT = "00"
              GO TO UPDATE-VOID.
           IF VDUBUF NOT = "     Y" AND
                      NOT = "     N"
              GO TO UPDATE-VOID.

           IF VDUBUF = "     N"
              MOVE "M" TO CX-CKPAID-NEW
              GO TO SKIP-PRINT-NEW-CHECK
           ELSE
              MOVE "S" TO CX-CKPAID-NEW.

       PRINT-REPLACEMENT-CHECK.
           PERFORM REQUEST-CX1-FILE.

           PERFORM UPDATE-FM1-FILE.

           PERFORM SET-LOANKEY.
           MOVE "END" TO IO-TYPE.
           GO TO EXIT-UPDATE-ROUTINE.

       SKIP-PRINT-NEW-CHECK.
           PERFORM OPEN-CX1-FILE.
           PERFORM UPDATE-FILES-CHECKTRANS.
           PERFORM CLOSE-CX1-FILE.
           MOVE "CLR" TO IO-TYPE.
           GO TO EXIT-UPDATE-ROUTINE.

      *********************************************
      *    C  A  S  H     R E  C  E  I  P T  S
      *********************************************
       UPDATE-CASH.
           MOVE "UPDATE-CASH  NOT VALID !" TO MESS.
           PERFORM SEND-MESS.

           MOVE "CLR" TO IO-TYPE.
           GO TO EXIT-UPDATE-ROUTINE.

      *********************************************
      *       R  S  V     R E  C  E  I  P T  S
      *********************************************
       UPDATE-RSVREC.

           MOVE "UPDATE-CASH  NOT VALID !" TO MESS.
           PERFORM SEND-MESS.
           MOVE "CLR" TO IO-TYPE.
           GO TO EXIT-UPDATE-ROUTINE.

      *********************************************
      *       B    A    N    K
      *********************************************
       UPDATE-BANK.
           MOVE "UPDATE-BANK  NOT VALID !" TO MESS.
           PERFORM SEND-MESS.
           MOVE "CLR" TO IO-TYPE.
           GO TO EXIT-UPDATE-ROUTINE.

       EXIT-UPDATE-ROUTINE.
           EXIT.

      *******************************************************
      * VERIFY PASSWORD SECTION
      *   --> ENTER PASSWORD IF ISSUING A NEW CHECK
      *******************************************************
       VERIFY-PASSWORD SECTION.
           MOVE " " TO PASSWDW-STAT.
           MOVE "VOIDI0" TO PASSWDW-PGM.
           MOVE "PLEASE ENTER THE PASSWORD," TO PASSWDW-PROMPT1.
           MOVE "CL/PASSWD" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH PASSWDW-BUF.
           CANCEL FORM-PROGX.

      ***************************************************
       REQUEST-CX1-FILE SECTION.
      ***************************************************

           MOVE "UPDATING CHECK REQUEST FILE............." TO MESS.
           PERFORM SEND-LEGEND.

      * REF = SEQNO AIDS LONPM2 IN LOCATING CX-RECORD;
      * CX-REF AND TT-CKWRITTEN ARE PASSED IN LOAN-KEY.

           MOVE TRANS-DATE TO CX-DATE.
           MOVE CX-REC TO SAVE-CX-REC.
           PERFORM OPEN-CX1-FILE.
           IF CXFILE-SCAN-FLAG = "Y"
              PERFORM SET-CX-REF
              MOVE SAVE-CX-REF TO CX-REF
              PERFORM REWRITE-CX1-FILE
              IF IO-FG = 9
                 GO TO IO-ERROR
              ELSE
                 GO TO EXIT-UPDATE-CX1.
           MOVE 1 TO SAVE-CX-SEQNO.
       CREATE-CX-TEMP.
           PERFORM SET-CX-REF.
           MOVE SAVE-CX-REC TO CX-REC.
           PERFORM READ-CX1-FILE.
           IF IO-FG = 9
              MOVE SAVE-CX-REC TO CX-REC
              PERFORM WRITE-CX1-FILE
              GO TO EXIT-UPDATE-CX1.
           IF SAVE-CX-SEQNO < 9999
              ADD 1 TO SAVE-CX-SEQNO
      *       UNLOCK CX-FILE
              GO TO CREATE-CX-TEMP.
           MOVE "REQUEST # = 9999, REQUEST NOT CREATED!" TO MESS.
           PERFORM SEND-MESS.
       EXIT-UPDATE-CX1.
           PERFORM CLOSE-CX1-FILE.


      ********************************************
       UPDATE-FM1-FILE SECTION.
      ********************************************

           IF CXFILE-SCAN-FLAG = "Y"
              GO TO EXIT-UPDATE-FM1.

           MOVE "UPDATING FORM REQUEST FILE............" TO MESS.
           PERFORM SEND-LEGEND.

           PERFORM OPEN-FM1-FILE.

           MOVE SPACES        TO LPWSFM-EDIT-FM-INITKEY.

           MOVE FM-PATH-OWNBR TO FM-BRNO.
           MOVE FD-FMNO       TO FM-FMNO LPWSFM-EDIT-FM-CKFMNO.
       NEXT-FM.
           MOVE SAVE-CX-BRNO           TO LPWSFM-EDIT-FM-BRNO.
           MOVE SAVE-CX-TYPE           TO LPWSFM-EDIT-FM-TYPE.
           MOVE SAVE-CX-SEQNO          TO LPWSFM-EDIT-FM-SEQNO.
           MOVE LPWSFM-EDIT-FM-INITKEY TO FM-INITKEY.
           PERFORM READ-FM1-FILE.
           IF IO-FG = 0
              ADD 1 TO LPWSFM-EDIT-FM-CKFMNO
              GO TO NEXT-FM.

           PERFORM CLEAR-FM-FILE.

           MOVE FM-PATH-OWNBR          TO FM-BRNO.
           MOVE LPWSFM-EDIT-FM-INITKEY TO FM-INITKEY.
           MOVE FD-FMNO                TO FM-FMNO.

           MOVE CX-ZIP        TO FM-ZIP.
           MOVE SPACES        TO FM-PRINT.
           MOVE 0             TO FM-STMT-COUNTER.
           MOVE TRANS-DATE    TO FM-TRDATE.
           MOVE EXT-CONAME-USER-ID TO FM-USERID.
           PERFORM WRITE-FM1-FILE.
           IF IO-FG NOT = 0
              GO TO IO-ERROR.

       EXIT-UPDATE-FM1.
           EXIT.

      *********************************
       NEW-RECORD-SETUP SECTION.
      *********************************

           PERFORM CLEAR-CX-FILE.

           MOVE BRANCH TO CX-BRNO.
           MOVE "N" TO CX-DT-UNPAID.
           MOVE "I0" TO CX-PGM.
           MOVE 1 TO NEW-FLAG.
           MOVE 0 TO ELE.

      *****************************
       DISPLAY-CK-FIELDS SECTION.
      *****************************

           MOVE DISPLAY-SEQ             TO PASSWDW-SEQ.
           MOVE "TO DISPLAY CHECK INFO" TO PASSWDW-PROMPT2.
           PERFORM VERIFY-PASSWORD.
           IF PASSWDW-STAT NOT = "OK"
              MOVE CK-PAID     TO CX-PAID TRANS-TYPE
              PERFORM VERIFY-TRANS-TYPE
              MOVE CK-TYPE     TO CX-TYPE CHECK-PAYEE
              PERFORM VERIFY-CHECK-PAYEE
              MOVE CK-DLRVNO   TO CX-DLRVNO DLRVNO
              MOVE CK-TILLNO   TO CX-TILLNO
              GO TO EXIT-DISPLAY-CK-FIELDS.

           MOVE CK-PAID     TO CX-PAID.
           MOVE CK-TYPE     TO CX-TYPE.
           MOVE CK-DLRVNO   TO CX-DLRVNO.
           MOVE CK-TILLNO   TO CX-TILLNO.
           PERFORM DISPLAY-SCREEN-HEADER.

           MOVE " " TO DISPLAY-BUF.

           IF (DEALER)
               MOVE CK-DLNO TO D-0101.

           MOVE CK-CLASS    TO D-020.
           MOVE CK-ACCTNO   TO L-ACCTNO.
           MOVE L-NUMBER    TO D-021.
           MOVE CK-PAYEE    TO D-030.
           MOVE CK-ADR1     TO D-040.
           MOVE CK-ADR2     TO D-050
           MOVE CK-CITY     TO D-060
           MOVE CK-STATE    TO D-070
           MOVE CK-ZIP      TO D-080

           MOVE CK-REASON   TO D-090.
           MOVE CK-DATE     TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO D-09A.
           MOVE CK-AMT      TO D-100.
           MOVE CK-REF      TO D-110.

           MOVE 0 TO SUB.
       NEXT-GL-DISP.
           ADD 1 TO SUB.
           IF CK-GLNO(SUB) NOT = 0
              MOVE CK-GLNO(SUB)    TO W-GLNO
              MOVE W-GLBR          TO D-GLBR(SUB)
              MOVE W-GLNUM         TO D-GLNUM(SUB)
              MOVE CK-GLAMT(SUB)   TO D-GLAMT(SUB).
           IF SUB < 5
              GO TO NEXT-GL-DISP.
           MOVE DISPLAY-BUF TO WR-BUF.
           MOVE DISPLAY-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

           PERFORM SEND-GLBAL.

       EXIT-DISPLAY-CK-FIELDS.
           EXIT.

      **********************************
      * PROCESS CK RECS BEFORE CHANGE  *
      **********************************
       PROCESS-OLD-RECORDS SECTION.
       ENTER-DEALER-NUM.
           MOVE "F1-CANCEL,  ENTER DEALER # " TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "OLD RECORDS" TO MESS PERFORM SEND-MESS.
           MOVE "EN N042010." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO EXIT-PROCESS-OLD-RECORDS.
           IF VDUSTATUS NOT = "00"
              GO TO ENTER-DEALER-NUM.
           MOVE VDUNUM2 TO CX-DLNO DL-DLNO CK-DLNO.
       EXIT-PROCESS-OLD-RECORDS.
           EXIT.

      *****************************************
       DISPLAY-DEALER-MASTER SECTION.
      *****************************************
           PERFORM OPEN-DL1-FILE.
           MOVE CX-DLNO TO DL-DLNO.
           PERFORM READ-DL1-FILE.
           PERFORM CLOSE-DL1-FILE.
           IF IO-FG = 9
              MOVE "INVALID DEALER NO. !!" TO MESS
              PERFORM SEND-MESS
              MOVE 9 TO IO-FG
              GO TO EXIT-DISPLAY-DEALER-MASTER.

           MOVE DL-DLNO TO D-0101.
      * ONLY USE THE DEALER MASTER TO POPULATE THE PAYEE FIELDS IF
      * ITS EQUAL TO THE CX-PAYEE. MULTIPLE CHECKS IN S/F ALLOW PAYEE'S
      * OTHER THAN THE DEALER. SO WHEN YOU VOID A CHECK WE NEED THE CORRECT
      * PAYEE FOR THE VOID AND/OR A REPLACEMENT CHECK
           IF CX-PAYEE = DL-NAME
              MOVE DL-NAME TO D-030
              MOVE DL-ADR1 TO D-040
              MOVE DL-ADR2 TO D-050
              MOVE DL-CITY TO D-060
              MOVE DL-STATE TO D-070
              MOVE DL-ZIP TO D-080
           ELSE
              MOVE CX-PAYEE TO D-030.

           PERFORM DISPLAY-NAME-ADDR.

           IF DL-TERMDATE < TRANS-DATE AND DL-TERMDATE NOT = 0
              MOVE " " TO MESS
              MOVE DL-TERMDATE TO DATE-YYYYMMDD
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY 
              MOVE DATE-MMDDYY TO VDU-DATE-EDIT
              STRING "DEALER TERMINATED ON " VDU-DATE-EDIT
                     ", POSTING IS NOT ALLOWED!"
                DELIMITED BY SIZE INTO MESS
              PERFORM SEND-MESS
              MOVE 9 TO IO-FG.

       EXIT-DISPLAY-DEALER-MASTER.
           EXIT.

      *****************************************
       CKFILE-SCAN SECTION.
      *****************************************
           MOVE "Y"       TO CKSCAN-NOSTOP-FG.
           MOVE BRANCH    TO CKSCANW-BRNO.
           MOVE 0         TO CKSCANW-NO.

           MOVE "WI/CKSCAN" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH CKSCAN-BUF EXIT-PATHNAME.
           CANCEL FORM-PROGX.
           IF CKSCAN-STATUS NOT = "00"
              MOVE 9 TO IO-FG
           ELSE
              MOVE 0 TO IO-FG
              MOVE CKSCAN-CK1-KEY TO CK1-KEY
              MOVE CK-NO TO CHECK-NO CX-CKNO
              MOVE CK-BRNO        TO CX-BRNO
              MOVE "SN N060K030." TO VDU
              MOVE CK-NO          TO VDUNUM0
              PERFORM SCREENIO.

      * COMMON UPDATE ROUTINES WITH ALL CHECK PRINTS
       COPY "LIBLP/CHECKTRANS.CPY".

       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/GPENV.CPY".
       COPY "LIBLP/LPMENU.CPY".
       COPY "LIBGB/DATER.CPY".
       COPY "LIBLP/POSTDATE.CPY".
       COPY "LIBLP/LPFMCL.CPY".
       COPY "LIBLP/LPCXCL.CPY".
       COPY "LIBLP/LPDTCL.CPY".
       COPY "LIBLP/LPDSCL.CPY".
       COPY "LIBLP/LPTRCCL.CPY".
       COPY "LIBLP/LPTYCL.CPY".
       COPY "LIBLP/LPCKCL.CPY".

      *********************************************
       FORM-RESET SECTION.
      *********************************************
           DISPLAY VOIDI-SCREEN UPON TCLP-MAIN-WINDOW-HANDLE.
           MOVE VOIDI-FORM-FIELDS TO WR-BUF.
           MOVE VOIDI-FORM-LIST   TO WR-LIST.
           PERFORM CALL-WRFORM.

      *********************************************
       SETUP-LINK-INFO SECTION.
      *********************************************
           IF VDU-FORM-NAME = "LPSMNU"
              MOVE LPSMNU-HELP--FILE TO HELP-LINK-FILE
              MOVE LPSMNU-HELP--DIR  TO HELP-LINK-DIR
           ELSE
              MOVE VOIDI-HELP--FILE TO HELP-LINK-FILE
              MOVE VOIDI-HELP--DIR TO HELP-LINK-DIR.

      *********************************************
       VDU-CONVERT-FIELD SECTION.
      *********************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBLP/VOIDI_EVA.CPY".
              COPY "LIBLP/LPSMNU_EVA.CPY".
              COPY "LIBLP/LPLNMU_EVA.CPY".
              COPY "LIBLP/LPCAMU_EVA.CPY".
              COPY "LIBLP/LPVDMU_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      *********************************
       MISC-PARAGRAPHS SECTION.
      *********************************
       COPY "LIBGB/GETPR.CPY".
       COPY "LIBGB/SYSTEM.CPY".
       COPY "LIBGB/GETENV.CPY".
       GET-FD1.
           MOVE IP-FMNO TO FD-FMNO.
           PERFORM OPEN-FD1-FILE.
           PERFORM READ-FD1-FILE.
           PERFORM CLOSE-FD1-FILE.
       GET-GLOBAL.
           PERFORM OPEN-GB-FILE.
           MOVE BRANCH TO GB-BRNO.
           MOVE 1      TO GB-NO.
           PERFORM READ-GB-FILE.
           PERFORM CLOSE-GB-FILE.
       GET-BRANCH.
           MOVE BRANCH TO BR-NO.
           PERFORM OPEN-BR-FILE.
           PERFORM READ-BR-FILE.
           PERFORM CLOSE-BR-FILE.
           IF IO-FG NOT = 0
              MOVE "BRANCH RECORD NOT FOUND" TO MESS
              PERFORM SEND-MESS
              GO TO IO-ERROR.
       SETUP-CX-FROM-CK.
           MOVE CK-TYPE       TO CX-TYPE.
           MOVE CK-AMT        TO CX-AMT.
           MOVE CK-PAYEE      TO CX-PAYEE.
           MOVE CK-REF        TO CX-REF.
           MOVE CK-REASON     TO CX-REASON.
           MOVE CK-VOID       TO CX-VOID.
      * CX-TYPE, CX-PAID SET IN DISPLAY-CK-FIELDS
      * RESET TO "VOID"
           MOVE HOLD-TRANS-TYPE TO CX-PAID.
           MOVE CK-CLASS      TO CX-CLASS.
           IF CK-DLNO = 202020
              MOVE 0             TO CX-DLNO
           ELSE
              MOVE CK-DLNO       TO CX-DLNO.
           MOVE CK-PGM        TO CX-PGM.
           MOVE CK-ACCTNO     TO CX-ACCTNO.
           MOVE CK-DLRVNO     TO CX-DLRVNO.
           MOVE CK-SSNO       TO CX-SSNO.
           IF CK-BATCHID = 0202020202
              MOVE 0 TO CK-BATCHID.
           MOVE CK-BATCHID    TO CX-BATCHID.
           MOVE CK-UNPAID     TO CX-DT-UNPAID.
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 5
                 MOVE CK-GLNO(SUB)  TO CX-GLNO(SUB)
                 MOVE CK-GLAMT(SUB) TO CX-GLAMT(SUB)
                 END-PERFORM.
           MOVE CK-PAYEE-NO   TO CX-PAYEE-NO.

       DISPLAY-NAME-ADDR.
           MOVE D-030 TO CX-PAYEE.
           MOVE D-040 TO CX-ADR1.
           MOVE D-050 TO CX-ADR2.
           MOVE D-060 TO CX-CITY.
           MOVE D-070 TO CX-STATE.
           MOVE D-080 TO CX-ZIP.
           MOVE DISPLAY-BUF TO WR-BUF.
           MOVE DISPLAY-LIST2 TO WR-LIST.
           PERFORM CALL-WRFORM.

       SET-CX-REF.
           MOVE 99 TO L-NUMBER1.
           MOVE SAVE-CX-SEQNO TO L-NUMBER2.
           MOVE L-NUMBER TO SAVE-CX-REF.
       SET-LOANKEY.
      *    THESE FIELDS ALLOW M2 TO LOCATE CX-REC.
           MOVE CX-REF TO L-NUMBER.
           MOVE PAYEE-SUB TO L-PAYEE-SUB.
           MOVE L-ACCTNO TO LOAN-KEY.
       NOT-FOUND.
           MOVE "NOT FOUND" TO MESS.
           PERFORM SEND-MESS.
       RESET-SCREEN.
      *    CALL FRESET USING STAT.
           MOVE SPACES TO DISPLAY-BUF.
           MOVE 0 TO VDUNUM2.
           PERFORM REFRESH-SCREEN.
       REFRESH-SCREEN.
           PERFORM DISPLAY-SCREEN-HEADER.
           MOVE DISPLAY-BUF TO WR-BUF.
           MOVE DISPLAY-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
           MOVE "S  S072GLBAL." TO VDU.
           MOVE 0 TO VDUNUM2.
           PERFORM SCREENIO.
           MOVE "S  A000NEW." TO VDU.
           MOVE SPACES TO VDUBUF.
           PERFORM SCREENIO.
           MOVE "S  A000CKNO." TO VDU.
           MOVE SPACES TO VDUBUF.
           PERFORM SCREENIO.

       DISPLAY-SCREEN-HEADER.
           MOVE CX-PAID TO TRANS-TYPE.
           PERFORM VERIFY-TRANS-TYPE.

           MOVE CX-DLRVNO TO DLRVNO.

           MOVE CX-TYPE TO VDUBUF CHECK-PAYEE.
           PERFORM VERIFY-CHECK-PAYEE.

           MOVE DISPLAY-HEAD-BUF TO WR-BUF.
           MOVE DISPLAY-HEAD-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       SEND-PRU-FORM.
           MOVE "S  A000PRU." TO VDU.
           MOVE IP-PRU TO VDUBUF.
           PERFORM SCREENIO.
           MOVE "SN N030FM." TO VDU.
           MOVE IP-FMNO TO VDUNUM0.
           PERFORM SCREENIO.
       SEND-LEGEND.
           MOVE "S  A400MSEL." TO VDU.
           MOVE MESS TO VDUBUF.
           PERFORM SCREENIO.
       RESET-SEL.
           MOVE "S  A000SEL." TO VDU.
           MOVE SPACES TO VDUBUF.
           PERFORM SCREENIO.
       SEND-GLBAL.
           MOVE "S  S072GLBAL." TO VDU.
           PERFORM CHECK-GL-BALANCE.
           PERFORM SCREENIO.
       XFER-FM-FIELDS.
           MOVE FD-FMNO TO IP-FMNO.
      * IP-PROG INITIALLY M2.
           IF FD-PRCONT = "Y"
              MOVE "1" TO CONT-PRINT
           ELSE
              MOVE "0" TO CONT-PRINT.

      *********************************
       IO-ROUTINE SECTION.
      *********************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBLP/LPTYGS_SQL.CPY".
       COPY "LIBLP/LPTRCGS_SQL.CPY".
       COPY "LIBLP/LPLNGS_SQL.CPY".
       COPY "LIBLP/LPFMGS_SQL.CPY".
       COPY "LIBLP/LPFDGS_SQL.CPY".
       COPY "LIBLP/LPDTGS_SQL.CPY".
       COPY "LIBLP/LPDSGS_SQL.CPY".
       COPY "LIBLP/LPDLGS_SQL.CPY".
       COPY "LIBLP/LPCXGS_SQL.CPY".
       COPY "LIBLP/LPCKGS_SQL.CPY".
       COPY "LIBLP/LPCAGS_SQL.CPY".
       COPY "LIBGB/GBGBGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBPRINT.CPY".
       COPY "LIBGB/GBGBRN.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBLP/LPCA1IN.CPY".
       COPY "LIBLP/LPCK1IN.CPY".
       COPY "LIBLP/LPCX1IN.CPY".
       COPY "LIBLP/LPDL1RN.CPY".
       COPY "LIBLP/LPDT1IN.CPY".
       COPY "LIBLP/LPDT2IN.CPY".
       COPY "LIBLP/LPDT4IN.CPY".
       COPY "LIBLP/LPDS1IN.CPY".
       COPY "LIBLP/LPFD1RN.CPY".
       COPY "LIBLP/LPFM1IN.CPY".
       COPY "LIBLP/LPLN1RN.CPY".
       COPY "LIBLP/LPTRC1IN.CPY".
       COPY "LIBLP/LPTY1IN.CPY".
       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO ERRCD.

      ***************************************
       END-ROUTINE SECTION.
      ***************************************
           PERFORM PRU-OUT.
       END-PROG.
           PERFORM XFER-FM-FIELDS.
           PERFORM CLOSE-FILES.
       EXIT-PROG.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY VOIDI-SCREEN.
       STOP-RUN.
           EXIT PROGRAM.
