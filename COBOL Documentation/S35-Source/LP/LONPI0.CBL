       IDENTIFICATION DIVISION.
       PROGRAM-ID.      LONPI0.
       DATE-WRITTEN.    011585.
      *****************************************************
      *                 (LP)
      *  DESCRIPTION:   CASH DRAWER RECEIPTS
      *                        + DEALER RESERVE RECEIPTS
      *                        + DEALER PRINCIPAL RECEIPTS
      *
      * LONPI0 = CASH RECEIPTS 
      * DISBI0 = CASH DISBURSEMENTS  (WAS 1ONPI0)
      * CHCKI0 = CHECK WRITING       (WAS 2ONPI0)
      * VOIDI0 = VOID CHECKS         (WAS 3ONPI0)
      * BNKDI0 = BANK DEPOSITS       (WAS 4ONPI0)
      * BNKWI0 = BANK WITHDR         (WAS 5ONPI0)
      * BCHKI0 = BATCH CHECK WRITING (WAS 6ONPI0)
      *
      *  DEBIT CASH OR RESERVE
      *  CREDIT GLNO ENTERED
      ***  ALL NUMBERS ARE ENTERED AS + FIGURES
      *
      * REV:
      * 060595 BAH CHANGED FORM ALIGNED TO BE AN "A" IN THE
      *            GENERATE HARD COPY QUESTION
      * 060595 BAH ADDED TC-DLNO FOR THE EMCARG TO PRINT
      * 060895 BLL CHANGED TO USE NEW GL ENTRY ROUTINE
      * 060995 BAH CHANGED TO STOP ON PAYEE AND ADDRESS, BECAUSE MERCURY
      *            WILL BE WRITING TO THEMSELVES AND OTHER DEALERS # E047
      * MJD 062895 ADDED COPY MEMBERS LIBLP/POSTDATE AND POSTDATEW.
      *            SET TC-USERID, POSTTIME, AND POSTDATE.
      * MJD 091895 ADDED DS-DATE LOGIC
      * MJD 091995 ADDED LOGIC TO WRITE DS REC IF ONE DOES NOT EXIST.
      * MJD 100395 ADDED PW TO DEALER RESERVE 3.
      * JTG 110295 ADDED TEST ON GI-FILE RE: CLASS NO ENTRY
      * MJD 020896 ADDED TY-FILE & UPDATE OF DEALER RESERVE FIELDS IN
      *              TY-REC  (NET GAIN/LOSS AND NEW VOLUME).
      * MJD 030496 ADDED E-CODE TEST OF "22" ON WRITE-TY1-FILE.
      * JTG 073196 ADDED ZERO CLEAR OF CX-DT-SEQ
      * CLS 032497 ADDED TEST OF LC-SALEFIN, LC-SERVACCT FOR DEALER RESERVE
      *            RECEIPTS INTO SALES FINANCE CLASS ONLY
      * BLV 110397 MOVE IN BR-GL-TYPE-ALLOWED, BR-GL-ENTRY-DEFAULT TO
      *            ALLENT-BUF BEFORE GL DISTRIBUTION WINDOW
      * MJD 112597 EXPANDED CX-REC TO 426 BYTES
      * MJD 032098 ADDED GETENV(W) FOR USE IN LIBLP/POSTDATE
      * BAH 062698 TILLNO IS NOW ALPHA, ADDED CX-TILLNO AND TC-TILLNO
      *             BECAUSE IT USED TO STORE IT IN CX-ACCTNO
      *  GLB 990720 ADD CASH DRAWER PASSWORD OPTION TO ENTER DRAWER NO.
      *             BR-PASSWORD-DRAWER(Y/N) & CA-PASSWORD (4 CHAR PASSWORD)
      *             REGMGM PR# 901
      * GLB 000508 PL#: 283 ADD BACK IN LOGIC TO STORE ACCT# IN KEY
      *            IF VALID LOAN ENTERED IN REF;CUSTOM FROM A48
      *            FOR MULLEN; REQUESTED FROM LIBERAL FIN 71090
      * GLB 000810 DLR:SUBSTITUTE COPY MEMBER LPCXCL TO CLEAR CX-RECORD
      * GLB 010807 IF A "0" LOAN ACCOUNT ENTERED, SKIP VERIFICATION
      * BAH 011115 CHANGED RESET-SCREEN BACK TO FRESET !!
      * GLB 020114 NOW NEED TO ENTER WI/DTSCAN WITH A 0 DATE, NOT TRANS-DATE
      * GLB 020128 NO CHANGE TO SOURCE; TURN OFF ECHO IN FORM LP/LONPI
      * GLB 030325 WORLD PR# 320;RECOMPILE DUE TO CHECKTRANSW/CHECKTRANS
      * GLB 030325 BUG; LOAD DTSCAN-BUF WITH ACCTNO AND DATE
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
      *  CS 060606 ADDED CHECKS FOR GP-I-AM-SECURITY TO RESTRICT ENTRY OF
      *            RESERVES BASED ON DEALER / CLASS COMBO SECURITY AUTO #2503
      *  CS 060607 ADD NEW FIELD CX-EVENT-CD TO UPDATE DT-EVENT SEC AUTO #2503
      * BLF 060720 ADD PA-FILE FOR UPDATE OF YTD PAID AMT IN
      *            PAFILE IN CHECKTRANS, DEVELOPMENT PR# 2633
      *  CS 060731 CORRECTED TEST ON GP-I-AM-SECURITY ONLY ALLOW ENTRY OF
      *            EVENT CODE WITH RR RECEIPTS TO PRINCIPAL PR#2503
      * BAH 140814 ADDED TEST FOR LPSMNU IN HELP
      *  CS 150318 ADDED TO RESET-SCREEN, CLEAR TILL CKNO GLBAL, WAS LEAVING
      *            CASH DRAWER NUMBER DISPLAYED,GLBALANCE AND 'U' TO UPDATE
      * BAH 150713 CHANGED DIRECT-CHAN-TABLE IN CHECKTRANSW TO ALLOW YOU
      *            TO CHANGE FIELDS 5 THROUGH 8, ADR2 - ZIP, REGMGM PR# 4786
      *  BAH 160304 CHANGED TILLNO TO NUMERIC 
      *
      * KEC 2016.0325 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         CHANGED CDSCAN-WINDOW-BUF TO HAVE MATCHING CD1-KEY
      *         LAYOUTS (FOR EACH CODE TYPE; CD-TYPE).
      *         CDSCAN-CD1-KEY IS THE NEW (PRIMARY) WITH REDEFINES OF
      *         CDSCAN-??-KEY (NEW).
      * BLM 160513 FIX FOR NEW FORMAT OF DS-DATE, FIX DS-UNEARNED, PD0003
      * BAH 160713 DO NOT SWITCH TO A SERVICE CLASS AFTER "CLASS MISMATCH"
      *            ERROR MESSAGE FOR DLR RESERVE RECEIPTS, PEP PL#944
      * KEC 2017.0105 {PR#00000} PARADATA <A30> [JAY CISZEWSKI]
      *          CHANGED DS-DATE TO DS-DATE-CCYYMM FOR CONSISTENCY
      *          WITH THE DATE FIELDS IN BY & UD FILE.
      *
      * BAH 170629 ACTIVATE 8 DIGIT DATES, PD0006
      * BAH 181010 GLOBAL CXFILE
      * BAH 181029 GLOBAL DTFILE
      * BAH 181115 GLOBAL FMFILE
      * BAH 190312 MOVED EDIT-FM-INITKEY TO LPWSFM-EDIT-FM-INITKEY 
      * BAH 20190614 ADDED CLEAR-FM-FILE (LPFMCL) AND SET FM-USERID
      * BAH 20210303 ADDED "D" "DB" DEBIT CARD ENTRY FOR UMC #1490
      * BAH 2022.0314 REMOVED EXT-SPANISH-FORMS AND ALL MX FORMS
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
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)LP/LONPI0.CBL S35 02/20/24-15:27:29 >".
      **********************************
      *       F I L E   I F N          *
      **********************************
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBGB/GB01GB.CPY".
       COPY "LIBGB/GB01GB_SQL.CPY".
       COPY "LIBGB/GBWSGB.CPY".
       COPY "LIBGL/GL01GI.CPY".
       COPY "LIBGL/GL01GI_SQL.CPY".
       COPY "LIBGL/GLWSGI.CPY".
       COPY "LIBLP/LP01TRC.CPY".
       COPY "LIBLP/LP01TRC_SQL.CPY".
       COPY "LIBLP/LPWSTRC.CPY".
       COPY "LIBLP/LP01CD.CPY".
       COPY "LIBLP/LP01CD_SQL.CPY".
       COPY "LIBLP/LPWSCD.CPY".
       COPY "LIBLP/LP01BW.CPY".
       COPY "LIBLP/LP01BW_SQL.CPY".
       COPY "LIBLP/LPWSBW.CPY".
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
       COPY "LIBLP/LP01LC.CPY".
       COPY "LIBLP/LP01LC_SQL.CPY".
       COPY "LIBLP/LPWSLC.CPY".
       COPY "LIBLP/LP01LN.CPY".
       COPY "LIBLP/LP01LN_SQL.CPY".
       COPY "LIBLP/LPWSLN.CPY".
       COPY "LIBLP/LP01TY.CPY".
       COPY "LIBLP/LP01TY_SQL.CPY".
       COPY "LIBLP/LPWSTY.CPY".
       COPY "LIBLP/CHECKTRANSW.CPY".
       COPY "LIBLP/LPIPBUF.CPY".
       COPY "LIBGB/CAPASSWDW.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBGB/PASSWDW.CPY".
       01  DLRVNO3-SEQ            PIC 9  VALUE 0.
       COPY "LIBGB/SYSTEMW.CPY".

       COPY "LIBGL/ALLENTW.CPY".

       COPY "LIBGB/GPENVW.CPY".
       COPY "LIBLP/LPDSCLW.CPY".
       COPY "LIBLP/LPMENUW.CPY".
       COPY "LIBLP/LPSETTYW.CPY".
       COPY "LIBLP/POSTDATEW.CPY".
       COPY "LIBWI/DEBREFW.CPY".
       COPY "LIBWI/DTSCANW.CPY".
       COPY "LIBWI/DLSCANW.CPY".
       COPY "LIBWI/LCSCANW.CPY".
       COPY "LIBWI/BWSCANW.CPY".
       COPY "LIBWI/CDSCANW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBLP/LONPI_DEF.CPY".
       COPY "LIBLP/LPSMNU_DEF.CPY".
       COPY "LIBLP/LPLNMU_DEF.CPY".
       COPY "LIBLP/LPCAMU_DEF.CPY".
       COPY "LIBLP/LPVDMU_DEF.CPY".
       COPY "LIBLP/LONPI_WKS.CPY".
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
       COPY "LIBLP/LONPI_SCN.CPY".
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
           PERFORM CLOSE-LC1-FILE.
           PERFORM CLOSE-FM1-FILE.
           PERFORM CLOSE-FD1-FILE.
           PERFORM CLOSE-DT4-FILE.
           PERFORM CLOSE-DT2-FILE.
           PERFORM CLOSE-DT1-FILE.
           PERFORM CLOSE-DS1-FILE.
           PERFORM CLOSE-DL1-FILE.
           PERFORM CLOSE-CX1-FILE.
           PERFORM CLOSE-CK1-FILE.
           PERFORM CLOSE-CD1-FILE.
           PERFORM CLOSE-CA1-FILE.
           PERFORM CLOSE-BW1-FILE.
           PERFORM CLOSE-GI1-FILE.
           PERFORM CLOSE-GB-FILE.
           PERFORM CLOSE-BR-FILE.
           PERFORM CLOSE-PR-FILE.
       COPY "LIBGB/DECLAR2.CPY".

      ************************************************
      *   C O N T R O L   M O D U L E
      *
      *   TRANS-CODES:
      *         0      RECEIPT (CASH DRAWER)
      *         1      DISBURSMENT (CASH DRAWER)
      *         2      CHECK FUNCTIONS
      *         3      BANK WITHDRAWALS
      ************************************************
       MAIN-PROGRAM SECTION.

           PERFORM SQL-CONNECT.

      D    STOP MESS.

           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "LONPI" TO VDU-FORM-NAME.

           PERFORM FORM-RESET.

           PERFORM GET-GLOBAL.

           PERFORM GET-GPENV.

           PERFORM GET-BRANCH.

           MOVE 0             TO Q-POS C-POS.
           MOVE LONPI-PRU-POS TO P-POS.
           MOVE "N"           TO Q-BUF.
           MOVE "01"          TO C-BUF.

           MOVE "S  A020PRU." TO VDU.
           MOVE 00  TO VDUBUF P-BUF IP-PRU.
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

      *    CALL FLDVAL USING STAT IP-LIST IP-BUF.
      * INITIALIZE WHAT A15 FORM WOULD HAVE DONE WITH THE ABOVE FLDVAL
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
           MOVE "ENNN030FM." TO VDU.
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
           GO TO GET-FMNO.
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
              MOVE 1 TO FIRST-TIME
           ELSE
              PERFORM RESET-SCREEN.

           PERFORM ENTER-KEYS.
           IF IO-TYPE = "PRU"
              GO TO ENTER-FMNO.
           IF IO-TYPE = "CLR"
              GO TO MAIN-MODULE.
           IF IO-TYPE = "END"
              GO TO END-ROUTINE.

           PERFORM ENTRY-MODULE.

      *---------------------------------------------------------
      * CALL PRINT PROGRAM: LONPM2, LONP3T (WORLD)
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
      *    E N T E R   K E Y S           *
      ************************************

       ENTER-KEYS SECTION.
       RESEND-PRU-FMNO.
      *    PRU AND FMNO NOT REENTERED UPON F1 RESET.
           PERFORM SEND-PRU-FORM.
      *---------------------------------------------------------
      * ENTER TRANS TYPE; 
      *                  *R=RSV REC
      *                  *C=CASH REC, 
      *                  *D=DEBIT CARD (UMC), INTERNALLY CHANGE BACK TO "C"
      *                     WILL CHANGE BACK TO "C" SO IT CREATES A 
      *                     TR "C" CASH RECORD
      *---------------------------------------------------------
       ENTER-TRANS-TYPE.
           MOVE 0 TO NEW-FLAG CHECK-NO.
           MOVE SPACES TO IO-TYPE.

           PERFORM NEW-RECORD-SETUP.

           MOVE CX-PAID          TO HOLD-TRANS-TYPE.
           MOVE CX-TYPE          TO HOLD-CHECK-PAYEE.
           MOVE CX-DLRVNO        TO HOLD-DLRVNO.


      * F7-MENU, ^-BACK, C-CASH RECEIPTS, D-DEBIT, R-RESERVE RECEIPTS:
           MOVE LONPI0-MESS-1 TO MESS.
           PERFORM SEND-LEGEND.

           PERFORM DISPLAY-SCREEN-HEADER.

           MOVE "E  A010K010." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO EXIT-ENTER-KEYS.
           IF VDUSTATUS = "1>"
              GO TO MENU-KEY.
           IF VDUSTATUS = "1U"
              MOVE "PRU" TO IO-TYPE
              GO TO EXIT-ENTER-KEYS.
           IF VDUSTATUS NOT = "00"
              GO TO ENTER-TRANS-TYPE.
           IF VDUBUF = SPACES
              MOVE "S  A010K010." TO VDU
              MOVE HOLD-TRANS-TYPE TO VDUBUF
              PERFORM SCREENIO.

           MOVE VDUBUF TO TRANS-TYPE CX-PAID HOLD-TRANS-TYPE.

      *  TYPE-SUB 4 = DEALER RSV CASH RECEIPT
      *  TYPE-SUB 5 = CASH RECEIPT
           PERFORM VERIFY-TRANS-TYPE.
           IF TYPE-SUB > TYPE-MAX
              GO TO ENTER-TRANS-TYPE.

      * TRANS-TYPE = "D" IS A DEBIT CARD TRANSACTION, BUT WE NEED IT
      *                  TO ACT AS A "C" BUT WITH NO CASH, FORCE IT
      *                  BACK TO A "C" AND FORCE "DB" IN THE TYPE AND
      *                  THEN WE NOW TO FORCE ENTRY OF REFCD

           IF TRANS-TYPE = "D"
              MOVE "C" TO TRANS-TYPE CX-PAID HOLD-TRANS-TYPE
              MOVE "DB" TO CX-TYPE CHECK-PAYEE HOLD-CHECK-PAYEE
              PERFORM DISPLAY-SCREEN-HEADER
              MOVE CHFG-VAL(14) TO CHFG-TABLE
              GO TO ENTER-REFCD.

      *---------------------------------------------------------
      *---->  CUSTOM, FORCE PAYEE TYPE "CA" IF CASH RECEIPT
      *---------------------------------------------------------
           IF TRANS-TYPE = "C"
              MOVE "CA" TO CX-TYPE CHECK-PAYEE HOLD-CHECK-PAYEE
              PERFORM VERIFY-CHECK-PAYEE
              IF PAYEE-SUB > PAYEE-MAX
                 GO TO ENTER-TRANS-TYPE
              END-IF
              PERFORM DISPLAY-SCREEN-HEADER
              GO TO ENTER-TILLNO.

      *---------------------------------------------------------
      *---->  CUSTOM, REJECT IF NOT DEALER RSV REC
      *---------------------------------------------------------
           IF TRANS-TYPE NOT = "R"
              GO TO ENTER-TRANS-TYPE.

      *---------------------------------------------------------
       ENTER-PAYEE-TYPE.
      *---------------------------------------------------------
           MOVE
           "F7, ^, RR-RSV PRIN, RC-RSV CHGS, RP-PARTIC, RD-PROCEEDS:"
                 TO MESS.
           PERFORM SEND-LEGEND.

           MOVE "E  A020K020." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO EXIT-ENTER-KEYS.

           IF VDUSTATUS = "16"
              PERFORM ALIGNMENT-PRINT
              GO TO ENTER-PAYEE-TYPE.
           IF VDUSTATUS = "1>"
              GO TO MENU-KEY.
           IF VDUSTATUS = "1U"
              GO TO ENTER-TRANS-TYPE.
           IF VDUSTATUS NOT = "00"
              PERFORM DISPLAY-SCREEN-HEADER
              GO TO ENTER-PAYEE-TYPE.
           IF VDUBUF = SPACES
              MOVE HOLD-CHECK-PAYEE TO VDUBUF.

           MOVE VDUBUF TO CX-TYPE CHECK-PAYEE HOLD-CHECK-PAYEE.

           PERFORM VERIFY-CHECK-PAYEE.

      *---------------------------------------------------------
           IF PAYEE-SUB > PAYEE-MAX
              GO TO ENTER-PAYEE-TYPE.
           IF (DEALER-RSV-REC)
              GO TO OKAY-CHECK-PAYEE.
           IF (DEALER-PROC-REC)
              GO TO OKAY-CHECK-PAYEE.
           GO TO ENTER-PAYEE-TYPE.

       OKAY-CHECK-PAYEE.
           PERFORM DISPLAY-SCREEN-HEADER.

           IF (DEALER-PROC-REC)
              MOVE "S  A010K021." TO VDU
              MOVE SPACES TO VDUBUF
              PERFORM SCREENIO
              GO TO ENTER-TILLNO.

      *---------------------------------------------------------
      *  ENTER DEALER PRINCIPAL/RESERVE BUCKET
      *---------------------------------------------------------
       ENTER-DLRVNO.
           MOVE "F7-MENU, ^-BACK, ENTER DEALER RESERVE NO. (1-3):"
                 TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "SNNN010K021." TO VDU.
           MOVE "_" TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "ENNN010K021." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO EXIT-ENTER-KEYS.
           IF VDUSTATUS = "1>"
              GO TO MENU-KEY.
           IF VDUSTATUS = "1U"
              GO TO ENTER-PAYEE-TYPE.
           IF VDUSTATUS NOT = "00"
              GO TO ENTER-DLRVNO.

           MOVE VDUNUM0 TO CX-DLRVNO DLRVNO HOLD-DLRVNO.
           IF DLRVNO < 1 OR > 3
              GO TO ENTER-DLRVNO.

           IF DLRVNO = 3
              MOVE DLRVNO3-SEQ TO PASSWDW-SEQ
              MOVE "TO ACCESS RESERVE 3" TO PASSWDW-PROMPT2
              PERFORM VERIFY-PASSWORD
              IF PASSWDW-STAT NOT = "OK"
                 GO TO ENTER-DLRVNO.

      *---------------------------------------------------------
       ENTER-TILLNO.
      *---------------------------------------------------------
           MOVE "F7-MENU, ^-BACK, ENTER CASH DRAWER NO:" TO MESS.
           IF BR-PASSWORD-DRAWER = "Y"
              MOVE "F7-MENU, ^-BACK, ENTER CASH DRAWER PASSWORD:"
                   TO MESS.
           PERFORM SEND-LEGEND.

      * - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       ACCEPT-ENTER-TILLNO.
           MOVE CX-BRNO    TO CA-BRNO.
           MOVE TRANS-DATE TO CA-DATE.
           MOVE 0          TO CA-TILLNO.
           MOVE CA1-KEY TO HOLD-CA1-KEY.
           IF BR-PASSWORD-DRAWER = "Y"
              PERFORM ENTER-CASH-DRAWER-PASSWORD
              IF CAPASSWD-OKAY = "N"
                 GO TO ACCEPT-ENTER-TILLNO
              END-IF
              IF CAPASSWD-OKAY = "V"
                 GO TO ACCEPT-CHECK-VDUSTATUS
              END-IF
              IF CAPASSWD-OKAY = "Y"
                 MOVE "SNNN020TILL." TO VDU
                 MOVE CA-TILLNO TO VDUNUM0
                 PERFORM SCREENIO
              END-IF
           ELSE
              MOVE "EN N020TILL." TO VDU
              PERFORM SCREENIO.

       ACCEPT-CHECK-VDUSTATUS.
           IF VDUSTATUS = "1>"
              GO TO MENU-KEY.
           IF VDUSTATUS = "1U"
              IF TRANS-TYPE = "C"
                 MOVE "CLR" TO IO-TYPE
                 GO TO EXIT-ENTER-KEYS
              ELSE
                 GO TO ENTER-PAYEE-TYPE.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO EXIT-ENTER-KEYS.
           IF VDUSTATUS NOT = "00"
              GO TO ENTER-TILLNO.

           MOVE VDUNUM0 TO TILLNO CX-TILLNO.
           IF TILLNO = 0
              GO TO ENTER-TILLNO.

           PERFORM VERIFY-TILL.
           IF IO-FG = 9
              GO TO ENTER-TILLNO
           ELSE
              GO TO EXIT-ENTER-KEYS.

      * GET HERE FROM "D" "DB" DEBIT CARD 
       ENTER-REFCD.
           MOVE "F6-SCAN, F7-MENU, ^-BACK, ENTER REF CODE:" TO MESS.
           PERFORM SEND-LEGEND.

           MOVE "S  A060REFPROMPT." TO VDU.
           MOVE "REFCD:"            TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "E  A050REFCD." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>"
              GO TO MENU-KEY.
           IF VDUSTATUS = "1U"
              MOVE "CLR" TO IO-TYPE
              GO TO EXIT-ENTER-KEYS.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO EXIT-ENTER-KEYS.
           IF VDUSTATUS = "1<"
              PERFORM CODE-SCAN
              IF IO-FG NOT = 0
                  GO TO ENTER-REFCD
              ELSE
                  MOVE "00" TO VDUSTATUS.
           IF VDUSTATUS NOT = "00"
              GO TO ENTER-REFCD.

           MOVE VDUBUF TO CX-FREE1.

           PERFORM OPEN-CD1-FILE.
           MOVE "BR"     TO CD-TYPE.
           MOVE CX-FREE1 TO CD-CODE.
           PERFORM READ-CD1-FILE.
           PERFORM CLOSE-CD1-FILE.
           IF (IO-BAD) OR (CD-BR-FILE-TYPE NOT = "C")
               MOVE "INVALID REFERENCE CODE" TO MESS
               PERFORM SEND-MESS
               GO TO ENTER-REFCD.

           GO TO EXIT-ENTER-KEYS.

      *---------------------------------------------------------
      *   E X I T      P R O G R A M
      *---------------------------------------------------------
       MENU-KEY.
           PERFORM PRU-OUT.
           DESTROY ALL CONTROLS.
           PERFORM MENU-SELECTION.
           IF MENU-FG = 99
              GO TO ENTER-TRANS-TYPE
           ELSE
              MOVE 0 TO LOAN-KEY
              MOVE "R" TO ROUTE-CD
              MOVE "END" TO IO-TYPE
              GO TO END-PROG.
       EXIT-ENTER-KEYS.
           EXIT.

      *    ************************************************
      *    *   ENTRY-MODULE
      *    *   NEW-FLAG = 0 - MODIFY EXISTING RECORD
      *    *              1 - INITIAL NEW RECORD ENTRY
      *    *              2 - MODIFY NEW RECORD
      *    ************************************************
       ENTRY-MODULE SECTION.

           MOVE " " TO MESS.
           PERFORM SEND-LEGEND.
           MOVE SPACES TO IO-TYPE.
           MOVE 1 TO ONE.
           MOVE FIRST-ELE TO ELE.
           MOVE 0 TO FIRST-ELE.
       CREATE-ROUTINE.
           IF NEW-FLAG NOT = 1
              GO TO MODIFY-ROUTINE.

           ADD ONE TO ELE.
           IF ELE < 1
              MOVE 1 TO ELE ONE.
           IF ELE NOT > MAX
              GO TO ENTER-ELE.

           MOVE 2 TO NEW-FLAG.
       MODIFY-ROUTINE.
           MOVE "F1-CANCEL, U-UPDATE, ENTER FIELD #:" TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "R RA060SEL." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF VDUSTATUS NOT = "00"
              GO TO MODIFY-ROUTINE.

           MOVE VDUBUF TO ACTION.

       ACTION-UPDATE.
           IF ACTION NOT = "     U"
              GO TO ACTION-FIELD.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * TEST FOR LOAN IN RESCISSION AND IF 'CHECK' PROGRAM SELECTED
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF CX-IN-RESCISSION
              MOVE MESSAGE-RESCISSION TO MESS
              PERFORM SEND-MESS
              GO TO MODIFY-ROUTINE.

           PERFORM CHECK-GL-BALANCE.
           IF BAL-FG  = 9
              IF TRANS-TYPE = "R"
                 MOVE "DEALER G/L MUST NET TO ZERO" TO MESS
              ELSE
                 MOVE "CASH NOT = G/L DISTRIBUTION" TO MESS
              END-IF
              PERFORM SEND-MESS
              GO TO MODIFY-ROUTINE.

      *---------------------------------------------------------
      *    U P D A T E  /  P R I N T    H A R D   C O P Y
      *---------------------------------------------------------
           PERFORM UPDATE-ROUTINE.

           GO TO END-ENTRY-MODULE.

       ACTION-FIELD.
           INSPECT ACTION REPLACING ALL " " BY "0".
           IF CHNO NOT NUMERIC
              GO TO MODIFY-ROUTINE.
           IF CHNO = 0
              GO TO MODIFY-ROUTINE.
           IF CHNO > MAX
              GO TO MODIFY-ROUTINE.
           MOVE CHNO TO ELE.
       ENTER-ELE.
           IF CHFG(ELE) = "N"
              IF NEW-FLAG = 1
                 GO TO CREATE-ROUTINE
              ELSE
                 GO TO MODIFY-ROUTINE.
           MOVE SPACES TO MESS.
           IF ELE = 1
              MOVE 0 TO HOLD-DT-DLNO
              IF (DEALER)
                 MOVE "F1, F6-SCAN, ENTER DEALER NO:" TO MESS
                 PERFORM SEND-LEGEND.
           IF ELE = 2
              MOVE "F1, F5-SCAN CLASS, ENTER LOAN CLASS:" TO MESS
              IF (DEALER)
                 MOVE
              "F1, F5-SCAN CLASS, F6-DEALER DETAIL, ENTER LOAN CLASS:"
                 TO MESS
              END-IF
              PERFORM SEND-LEGEND.
           IF ELE = 3
              MOVE "F1, ENTER PAYEE NAME:" TO MESS
              PERFORM SEND-LEGEND.
           IF ELE = 4
              MOVE "F1, ENTER PAYEE ADDRESS #1:" TO MESS
              PERFORM SEND-LEGEND.
           IF ELE = 5
              MOVE "F1, ENTER PAYEE ADDRESS #2:" TO MESS
              PERFORM SEND-LEGEND.
           IF ELE = 6
              MOVE "F1, ENTER PAYEE CITY:" TO MESS
              PERFORM SEND-LEGEND.
           IF ELE = 7
              MOVE "F1, ENTER PAYEE STATE:" TO MESS
              PERFORM SEND-LEGEND.
           IF ELE = 8
              MOVE "F1, ENTER PAYEE ZIP CODE:" TO MESS
              PERFORM SEND-LEGEND.
           IF ELE = 9
              MOVE "F1, ENTER REASON FOR RECEIPT:" TO MESS
              PERFORM SEND-LEGEND.
           IF ELE = 10
              MOVE "F1, ENTER AMOUNT OF THE RECEIPT:" TO MESS
              PERFORM SEND-LEGEND.
           IF ELE = 11
              MOVE "F1, ENTER CHECK REFERENCE NO:" TO MESS
              PERFORM SEND-LEGEND.

           IF ELE = 12 OR 13 OR 14 OR 15 OR 16
              PERFORM ENTER-GL
              GO TO CREATE-ROUTINE.

           IF ELE = 17
              MOVE "F1, ENTER FIRST LINE FREE TYPING:" TO MESS
              PERFORM SEND-LEGEND.
           IF ELE = 18
              MOVE "F1, ENTER SECOND LINE FREE TYPING:" TO MESS
              PERFORM SEND-LEGEND.

           MOVE SPEC(ELE) TO VDU.
           MOVE ELE TO VDUROW.
           MOVE 0 TO VDUCOL.
           MOVE "." TO VDUDECIMAL.
           PERFORM SCREENIO.
       TEST-F1.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
       TEST-UP.
           IF VDUSTATUS NOT = "1U"
              GO TO TEST-DOWN.
           IF NEW-FLAG = 1
              MOVE -1 TO ONE
              GO TO CREATE-ROUTINE.
       TEST-DOWN.
           IF VDUSTATUS NOT = "1D"
              GO TO TEST-FKEYS.
           MOVE 1 TO ONE.
           GO TO GOTO-FIELD.

       TEST-FKEYS.

       TEST-F5.
           IF VDUSTATUS NOT = "1:"
              GO TO TEST-F6.
           IF ELE NOT = 2
              GO TO TEST-F6.
           PERFORM CLASS-SCAN.
           IF IO-FG = 9
              GO TO ENTER-ELE.

       TEST-F6.
           IF VDUSTATUS NOT = "1<"
              GO TO TEST-F7.
           IF ELE = 1
              IF (DEALER)
                 PERFORM DEALER-SCAN
                 IF IO-FG = 9
                    GO TO ENTER-ELE
                 END-IF
              ELSE
              IF PAYEE-SUB = 1
                 PERFORM BORROWER-SCAN
                 IF IO-FG = 9
                    GO TO ENTER-ELE.

           IF ELE NOT = 2
              MOVE 1 TO ONE
              GO TO GOTO-FIELD.

      *----------------------------------------------
      *    SCAN DETAIL DT-FILE FOR DEALER LOAN#
      *----------------------------------------------
           MOVE 3           TO DTSCANW-TYPE.
           MOVE CX-DLNO     TO DTSCANW-DLNO.
           MOVE CX-CLASS    TO DTSCAN-CLASS.
           MOVE CX-ACCTNO   TO DTSCAN-ACCTNO.
           MOVE BRANCH      TO DTSCAN-BRNO.
           MOVE 0           TO DTSCAN-DATE.
           MOVE "WI/DTSCAN " TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH DTSCAN-BUF EXIT-PATHNAME.
           CANCEL FORM-PROGX.
           IF DTSCAN-STATUS NOT = "00"
              GO TO ENTER-ELE.

           MOVE DTSCAN-DT1-KEY TO DT1-KEY HOLD-DT1-KEY.

           PERFORM DISPLAY-DEALER-MASTER.
           IF IO-FG = 9
              GO TO ENTER-ELE.

           PERFORM OPEN-DT1-FILE.
           PERFORM READ-DT1-FILE.
           PERFORM CLOSE-DT1-FILE.
           IF IO-FG = 9
              MOVE "UNABLE TO LOCATE DETAIL FROM SCAN "
              TO MESS PERFORM SEND-MESS
              GO TO ENTER-ELE.

           PERFORM VERIFY-LOAN-CLASS.
      *  LOAN MASTER NOT VALID
           IF IO-FG = 9
              MOVE 0 TO HOLD-DT-DLNO
              GO TO ENTER-ELE.

           PERFORM SETUP-CX-FROM-DT.
           PERFORM DISPLAY-DEALER-STATS.
           PERFORM DISPLAY-SCREEN-BODY.

           MOVE 1 TO ONE.
           GO TO GOTO-FIELD.

       TEST-F7.
           IF VDUSTATUS NOT = "1>"
              GO TO TEST-00.
           PERFORM PRU-OUT.
           DESTROY ALL CONTROLS
           PERFORM MENU-SELECTION.
           IF MENU-FG = 99
              GO TO ENTER-ELE.
           MOVE 0 TO LOAN-KEY.
           MOVE "R" TO ROUTE-CD.
           MOVE "END" TO IO-TYPE.
           GO TO END-PROG.
       TEST-00.
           IF VDUSTATUS NOT = "00"
              GO TO ENTER-ELE.
       GOTO-FIELD.
           MOVE 1 TO ONE.
           GO TO F-01 F-02 F-03 F-04 F-05 F-06 F-07 F-08 F-09 F-10
                 F-11 F-12 F-13 F-14 F-15 F-16 F-17 F-18
                 DEPENDING ON ELE.
           GO TO END-ENTRY-MODULE.

      *************************************
      *    S E T   E L E M E N T S        *
      *************************************
       F-01.
           IF (DEALER)
              GO TO F-01DL
           ELSE
              GO TO F-01LN.
       F-01ERR.
           MOVE "INVALID PAYEE TYPE" TO MESS.
           PERFORM SEND-MESS.
           MOVE "E" TO ERRCD.
           GO TO END-ROUTINE.
       F-01LN.
           MOVE VDUNUM0 TO BW-SSNO.

           PERFORM DISPLAY-BORROWER.
           IF IO-FG = 9
              GO TO ENTER-ELE.
           MOVE BW-SSNO TO CX-SSNO.

           GO TO CREATE-ROUTINE.

       F-01DL.
           IF VDUNUM0 = 0
              MOVE CX-DLNO TO DL-DLNO
           ELSE
              MOVE VDUNUM0 TO DL-NUM
              COMPUTE CX-DLNO = DL-NUM / 100.

           PERFORM DISPLAY-DEALER-MASTER.
           IF IO-FG = 9
              GO TO ENTER-ELE.
           MOVE DL-DLNO TO CX-DLNO.

           GO TO CREATE-ROUTINE.

       F-02.
      * CLASS NEEDED FOR TT-FILE, FOR NOW.
           IF VDUNUM0 = 0
              MOVE "SN N020020." TO VDU
              MOVE CX-CLASS TO VDUNUM0
              PERFORM SCREENIO.
           MOVE VDUNUM0 TO CX-CLASS LC-CODE D-020.
           PERFORM GET-LC.
           IF IO-FG = 9
              MOVE "INVALID CLASS" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           IF (DEALER)
              IF LC-SALEFIN NOT = "Y"
                 MOVE "MUST BE SALES FINANCE CLASS!" TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-ELE
              END-IF
              IF (DEALER-RSV-REC)
                 IF LC-SERVACCT = "Y"
                    MOVE "CAN'T BE A SERVICE ACCOUNT CLASS!" TO MESS
                    PERFORM SEND-MESS
                    GO TO ENTER-ELE.


      *  FROM CASH RECEIPTS 
      *  DONT ALLOW DEALER RESERVE RECEIPTS TO DIRECT OR SERVICE CLASS
           IF (DEALER-RSV-REC)
              IF ( LC-SALEFIN NOT = "Y" ) OR
                 ( LC-SERVACCT = "Y" )
                 MOVE "DEALER RESERVE RECEIPTS INTO SALES CLASS ONLY"
                     TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.

           MOVE "S  A150CLDESC." TO VDU.
           MOVE LC-DESC TO VDUBUF D-CLDESC.
           PERFORM SCREENIO.
           MOVE BRANCH TO GI-BRANCH.
           MOVE LC-CODE TO GI-CLASS.
           PERFORM OPEN-GI1-FILE.
           PERFORM READ-GI1-FILE.
           PERFORM CLOSE-GI1-FILE.
           IF IO-FG = 9
               MOVE "NO GENERAL LEDGER INTERFACE!" TO MESS
               PERFORM SEND-MESS
               GO TO ENTER-ELE.

           MOVE LC-SERVACCT TO CX-SERVACCT.

           IF (NOT DEALER)
              GO TO CREATE-ROUTINE.

       F-021.
           MOVE "ENTER LOAN NO.,  ^ BACK TO CLASS:" TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "ENNN060021." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF VDUSTATUS NOT = "00"
              GO TO F-021.

           MOVE VDUNUM0 TO CX-ACCTNO CX-REF D-021.

      * FIND LOAN MASTER, 0 = VALID
           PERFORM VERIFY-LOAN-CLASS.
           IF IO-FG = 9
              MOVE 0 TO HOLD-DT-DLNO
              GO TO ENTER-ELE.

           PERFORM DISPLAY-DEALER-STATS.

      *  DTSCAN WAS USED, DT-FILE IDENTIFIED, LOAN MASTER FOUND
           IF IO-FG = 0
              IF HOLD-DT-DLNO NOT = 0
                 GO TO CONTINUE-F-021.

      * OPERATOR CHOSE NOT TO ENTER A SPECIFIC LOAN NO.
           IF CX-ACCTNO = 0
              PERFORM SETUP-DT-FROM-CX
                 GO TO CONTINUE-F-021.

      * LOAN NO. ENTER, TRY TO FIND DT-RECORD
           PERFORM TRY-TO-FIND-DT.
           IF IO-FG = 9
              MOVE "CANT FIND DEALER LOAN DETAIL" TO MESS
                 PERFORM SEND-MESS
              PERFORM SETUP-DT-FROM-CX.

       CONTINUE-F-021.
           PERFORM SETUP-CX-FROM-DT.

       CONTINUE-F-021B.
           PERFORM DISPLAY-SCREEN-BODY.
           PERFORM SEND-GLBAL.

      * GLB 08/07/01
           IF CX-ACCTNO = 0
              GO TO CREATE-ROUTINE.

           PERFORM DETAIL-STATUS-CHECK.
           IF IO-FG = 9
                 MOVE 0 TO HOLD-DT-DLNO
                 GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.

       F-03.
           MOVE VDUBUF TO CX-PAYEE.
      *    MUST ENTER PAYEE
           IF VDUBUF = " "
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       F-04.
           MOVE VDUBUF TO CX-ADR1.
           GO TO CREATE-ROUTINE.
       F-05.
           MOVE VDUBUF TO CX-ADR2.
           GO TO CREATE-ROUTINE.
       F-06.
           MOVE VDUBUF TO CX-CITY.
           GO TO CREATE-ROUTINE.
       F-07.
           MOVE VDUBUF TO CX-STATE.
           GO TO CREATE-ROUTINE.
       F-08.
           MOVE VDUBUF TO CX-ZIP.
           GO TO CREATE-ROUTINE.
       F-09.
           IF VDUBUF = SPACES
              MOVE "S  A000090." TO VDU
              MOVE CX-REASON TO VDUBUF
              PERFORM SCREENIO.
           MOVE VDUBUF TO CX-REASON.
           GO TO CREATE-ROUTINE.
       F-10.
           IF VDUNUM2 = 0
              MOVE "S  S072100." TO VDU
              MOVE CX-AMT TO VDUNUM2
              PERFORM SCREENIO.
           MOVE VDUNUM2 TO CX-AMT.
           IF VDUNUM2 = 0
               GO TO ENTER-ELE.

           PERFORM SEND-GLBAL.
           GO TO CREATE-ROUTINE.
       F-11.
           IF VDUNUM0 = 0
              MOVE "SN N060110." TO VDU
              MOVE CX-REF TO VDUNUM0
              PERFORM SCREENIO.
           MOVE VDUNUM0 TO CX-REF.
           GO TO CREATE-ROUTINE.

       F-111.
           MOVE "E  A020111." TO VDU.
           PERFORM SCREENIO.
           IF UP-KEY
              GO TO ENTER-ELE.

           IF F6-KEY
              PERFORM REFNO-SCAN.

           IF NOT(ENTER-KEY OR DOWN-KEY)
              GO TO F-111.

           MOVE VDUBUF TO CX-EVENT-CD.

      *  PER LORI VALIDATE
           MOVE  "BS"       TO CD-TYPE.
           MOVE CX-EVENT-CD TO CD-CODE.
           PERFORM OPEN-CD1-FILE.
           PERFORM READ-CD1-FILE.
           PERFORM CLOSE-CD1-FILE.
           IF IO-BAD
               MOVE "INVALID EVENT CODE" TO MESS
               PERFORM SEND-MESS
               GO TO F-111.

           GO TO CREATE-ROUTINE.

       F-12.
      *    PERFORM CHECK-GL.
      *    IF IO-FG = 9
      *       GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       F-13.
      *    PERFORM CHECK-GL.
      *    IF IO-FG = 9
      *       GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       F-14.
      *    PERFORM CHECK-GL.
      *    IF IO-FG = 9
      *       GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       F-15.
      *    PERFORM CHECK-GL.
      *    IF IO-FG = 9
      *       GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       F-16.
      *    PERFORM CHECK-GL.
      *    IF IO-FG = 9
      *       GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       F-17.
           MOVE VDUBUF TO CX-FREE1.
           GO TO CREATE-ROUTINE.
       F-18.
           MOVE VDUBUF TO CX-FREE2.
           GO TO CREATE-ROUTINE.
       END-ENTRY-MODULE.
           EXIT.

      ****************************************
      *    LOAN CLASS SCAN
      ****************************************
       CLASS-SCAN SECTION.
           MOVE 0      TO LCSCAN-KEY.
           MOVE SPACES TO LCSCAN-VERIFY.
           MOVE "Y"    TO LCSCAN-SALEFIN.
           MOVE "N"    TO LCSCAN-SERVACCT.
           MOVE "WI/LCSCAN" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH LCSCAN-BUF EXIT-PATHNAME.
           CANCEL FORM-PROGX.
           IF LCSCAN-STATUS NOT = "00" OR LCSCAN-KEY = 0
               MOVE 9 TO IO-FG
               GO TO EXIT-CLASS-SCAN.
           MOVE "SNNN020020." TO VDU.
           MOVE LCSCAN-KEY TO VDUNUM0.
           PERFORM SCREENIO.
           MOVE LCSCAN-KEY TO VDUNUM0.
           MOVE 0 TO IO-FG.

       EXIT-CLASS-SCAN.
           EXIT.

      ******************************************************************
       CODE-SCAN SECTION.
      ******************************************************************

           MOVE " "         TO DEBREF-BUF.
           MOVE "BR"        TO DEBREF-TYPE.

           MOVE "WI/DEBREF " TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH DEBREF-BUF EXIT-PATHNAME.
           CANCEL FORM-PROGX.

           IF DEBREF-CODE = SPACES
              MOVE 9 TO IO-FG
              GO TO EXIT-CODE-SCAN.

           MOVE "S  A050REFCD." TO VDU.
           MOVE DEBREF-CODE     TO VDUBUF.
           PERFORM SCREENIO.

       EXIT-CODE-SCAN.
           EXIT.

      ****************************************
      *    DEALER SCAN
      ****************************************
       DEALER-SCAN SECTION.
           MOVE 0 TO DLSCAN-NUM.
           MOVE "WI/DLSCAN " TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH DLSCAN-BUF EXIT-PATHNAME.
           CANCEL FORM-PROGX.
           IF DLSCAN-STATUS NOT = "00" OR DLSCAN-NUM = " "
               MOVE 9 TO IO-FG
               GO TO EXIT-DEALER-SCAN.
           MOVE "SN N042010." TO VDU.
           MOVE DLSCAN-DLNO TO VDUNUM2.
           PERFORM SCREENIO.
           MOVE DLSCAN-DLNO TO VDUNUM2.
           MOVE 0 TO IO-FG.

       EXIT-DEALER-SCAN.
           EXIT.


      **********************************************
      *    REFNO SCAN FOR VERIFY OF REFERENCE NO.
      **********************************************
       REFNO-SCAN SECTION.

           MOVE "BS"   TO CDSCAN-TYPE.
           MOVE SPACES TO CDSCAN-CODE.

           MOVE "WI/CDSCAN" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH CDSCAN-BUF EXIT-PATHNAME.
           CANCEL FORM-PROGX.
           IF CDSCAN-STATUS NOT = "00" OR CDSCAN-CODE = SPACES
              MOVE 9 TO IO-FG
              GO TO REFNO-SCAN-EXIT.

           MOVE 0 TO IO-FG.

           MOVE "S  A050111."   TO VDU.
           MOVE CDSCAN-CODE TO VDUBUF.
           PERFORM SCREENIO.

           MOVE CDSCAN-CODE TO VDUBUF.

       REFNO-SCAN-EXIT.
           EXIT.

      **************************************
      *    BORROWER WINDOW SCAN
      **************************************
       BORROWER-SCAN SECTION.
           MOVE 0   TO BWSCAN-NO.
           MOVE "S" TO BWSCAN-SCAN-TYPE.
           MOVE "WI/BWSCAN" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH BWSCAN-BUF EXIT-PATHNAME.
           CANCEL FORM-PROGX.
           IF BWSCAN-STATUS NOT = "00" OR BWSCAN-NO = 0
              MOVE 9 TO IO-FG
              GO TO EXIT-BORROWER-SCAN.
           MOVE BWSCAN-NO TO VDUNUM0.
           MOVE 0 TO IO-FG.

       EXIT-BORROWER-SCAN.
           EXIT.

      ***********************************
      *  VERIFY TILL ENTERED FOR RESERVE
      ***********************************
       VERIFY-TILL SECTION.

           MOVE CX-BRNO    TO CA-BRNO.
           MOVE TRANS-DATE TO CA-DATE.
           MOVE TILLNO     TO CA-TILLNO.
           PERFORM OPEN-CA1-FILE.
           PERFORM READ-CA1-FILE.
           PERFORM CLOSE-CA1-FILE.
           IF IO-FG = 9 OR CA-CLOSED-HHMM NOT = 0
              MOVE "CASH DRAWER NOT OPEN" TO MESS
              PERFORM SEND-MESS
              MOVE 9 TO IO-FG.

       EXIT-VERIFY-TILL.
           EXIT.


      ***********************************
       VERIFY-LOAN-CLASS SECTION.
      ***********************************
           IF CX-ACCTNO = 0
              MOVE 0 TO CX-SSNO
              GO TO EXIT-VERIFY-LOAN-CLASS.

           MOVE CX-ACCTNO TO LN-ACCTNO.
           PERFORM GET-LN.
           IF IO-FG = 9
              MOVE "UNABLE TO LOCATE LOAN !" TO MESS
              PERFORM SEND-MESS
              MOVE 9 TO IO-FG
              GO TO EXIT-VERIFY-LOAN-CLASS.

           IF CX-CLASS NOT = LN-CLASS
      * DEALER RESERVE RECEIPTS MUST HAVE SALES FINANCE CLASS ONLY, CAN NOT
      * AUTOMATICALLY SWITCH THEM TO THE LOAN CLASS WITHOUT VERIFYING! #944
              IF NOT (DEALER-RSV-REC)
                 PERFORM SWITCH-CLASS
              ELSE
                 MOVE LN-CLASS TO LC-CODE
                 PERFORM GET-LC
                 IF IO-FG = 9
                    MOVE "MISSING CLASS ON THIS LOAN" TO MESS
                    PERFORM SEND-MESS
                    MOVE 9 TO IO-FG
                    GO TO EXIT-VERIFY-LOAN-CLASS
                 ELSE
                    IF (LC-SALEFIN NOT = "Y") OR (LC-SERVACCT = "Y")
                       MOVE "LOAN CLASS IS SERVICE OR NOT SALES" TO MESS
                       PERFORM SEND-MESS
                       MOVE 9 TO IO-FG
                       GO TO EXIT-VERIFY-LOAN-CLASS
                    ELSE
                       PERFORM SWITCH-CLASS.


           IF (DEALER)
              IF CX-DLNO NOT = LN-DLNO
                 MOVE LN-DLNO TO CX-DLNO D-0101
               MOVE "DEALER MISMATCH!, SWITCHING TO LOAN DEALER &&&&&&&"
                 TO MESS
                 INSPECT MESS REPLACING FIRST "&&&&&&&" BY D-0101
                 PERFORM SEND-MESS
                 MOVE 8 TO IO-FG.

       EXIT-VERIFY-LOAN-CLASS.
           EXIT.

      *********************************
      *   V E R I F Y   T Y P E       *
      *   VERIFY TYPE CODE;           *
      *
      *   1 =  MANUAL CHECK
      *   2 =  SYSTEM CHECK
      *   3 =  VOID   CHECK           *
      *   4 =  DEALER RSV CASH REC    *
      *   5 =  CASH RECEIPT           *
      *   6 =  BANK TRANSACTION       *
      *   7 =  DEBIT CARD TRANSACTION *
      *
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
      * 1  LN, 2  DL, 3  MS, 4,  5,  6 DR, 7  DC, 8  DP, 9  VX, 10 RR
      * 11 RC, 12 RP, 13 RD, 14 CA, 15 BK, 16 DD, 17 ID, 18 IW, 19 DB
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

       EXIT-VERIFY-PAYEE.
           MOVE "S  A000P010." TO VDU.
           IF (DEALER)
              MOVE "DEALER" TO VDUBUF.

      *********************************
      *   E N T E R   G/L #   A M T   *
      *********************************
       ENTER-GL SECTION.
           MOVE CX-AMT TO ALLENT-DISBURSE.
           IF (DEALER-RSV) OR TRANS-TYPE = "R"
              MOVE 0 TO ALLENT-DISBURSE.

           MOVE BRANCH TO ALLENT-BRNO.
           MOVE BR-OTHER-BRANCH-GL TO ALLENT-OTHER-BR-FG.
           MOVE BR-GL-TYPE-ALLOWED TO ALLENT-GL-TYPE-ALLOWED.
           MOVE BR-GL-ENTRY-DEFAULT TO ALLENT-GL-ENTRY-DEFAULT.
           MOVE 5 TO ALLENT-NO-OF-ACCTS.
           MOVE "GL/ALLENT" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH ALLENT-BUF.
           CANCEL FORM-PROGX.
           MOVE 1 TO SUB.
           MOVE 12 TO SUB1.
       MOVE-GL-LOOP.
           MOVE ALLENT-BR-ACCOUNT(SUB) TO CX-GLNO(SUB) W-GLNO.
           MOVE "SN N040000." TO VDU.
           MOVE SUB1 TO VDUROW.
           MOVE W-GLBR TO VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SN N070000." TO VDU.
           MOVE SUB1 TO VDUROW.
           MOVE 1 TO VDUCOL.
           MOVE W-GLNUM TO VDUNUM0.
           PERFORM SCREENIO.
           MOVE "S  S072  2." TO VDU.
           MOVE SUB1 TO VDUROW.
           MOVE ALLENT-AMT(SUB) TO CX-GLAMT(SUB) VDUNUM2.
           PERFORM SCREENIO.
           IF SUB NOT = 5
              ADD 1 TO SUB SUB1
              GO TO MOVE-GL-LOOP.
           IF NEW-FLAG = 1
              MOVE 16 TO ELE.
           MOVE 1 TO ONE.
       ENTER-GL-EXIT.
           EXIT.

      *********************************
      *   C H E C K   G/L   B A L     *
      *********************************
       CHECK-GL-BALANCE SECTION.
       CHECK-GLAMTS.
           MOVE 0 TO BAL-FG VDUNUM2.

      *  TESTING DEALER CHECK
      *       OR DEALER RESERVE CASH RECEIPTS (SEE LONPI0)
           IF DEALER
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

      *********************************
      * U P D A T E   R O U T I N E S *
      *********************************
       UPDATE-ROUTINE SECTION.

           GO TO
              UPDATE-MANUAL UPDATE-SYSTEM
              UPDATE-VOID   UPDATE-RSVREC
              UPDATE-CASH   UPDATE-BANK
      * DEBIT CARDS
              UPDATE-CASH
                            DEPENDING ON TYPE-SUB.

      *    SEE VERIFY-TRANS-TYPE FOR DETAILS.
           MOVE "INVALID CHECK TYPE" TO MESS.
           PERFORM SEND-MESS.
           MOVE "E" TO ERRCD.
           GO TO END-ROUTINE.

      *    -----------------------------------
      *        N E W    C H E C K S
      *    -----------------------------------
       UPDATE-MANUAL.
      *---------------
           MOVE "UPDATE-MANUAL NOT VALID !" TO MESS.
           PERFORM SEND-MESS.
           MOVE "CLR" TO IO-TYPE.
           GO TO EXIT-UPDATE-ROUTINE.

       UPDATE-SYSTEM.
      *---------------
           MOVE "UPDATE-SYSTEM NOT VALID !" TO MESS.
           PERFORM SEND-MESS.
           MOVE "CLR" TO IO-TYPE.
           GO TO EXIT-UPDATE-ROUTINE.

      *    -----------------------------------
      *        V  O  I  D    C  H  E  C  K  S
      *    -----------------------------------
       UPDATE-VOID.
      *---------------
           MOVE "UPDATE-VOID NOT VALID !" TO MESS.
           PERFORM SEND-MESS.
           MOVE "CLR" TO IO-TYPE.
           GO TO EXIT-UPDATE-ROUTINE.

      *    -----------------------------------
      *    C  A  S  H     R E  C  E  I  P T  S
      *    -----------------------------------
       UPDATE-CASH.
      *---------------
           MULTIPLY -1 BY CX-AMT.
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 5
             MULTIPLY -1 BY CX-GLAMT(SUB)
           END-PERFORM.
           PERFORM UPDATE-TC-FILE.
      * DO NOT UPDATE CASH DRAWER FOR DEBIT CARDS
           IF CX-TYPE NOT = "DB"
              PERFORM UPDATE-CA-FILE.

           PERFORM HARD-COPY-YN.
           IF VDUBUF = "     N"
              MOVE "CLR" TO IO-TYPE
              GO TO EXIT-UPDATE-ROUTINE.

           PERFORM REQUEST-CX1-FILE.
           PERFORM UPDATE-FM1-FILE.
           PERFORM SET-LOANKEY.
           MOVE "END" TO IO-TYPE.
           GO TO EXIT-UPDATE-ROUTINE.

      *    -----------------------------------
      *       R  S  V     R E  C  E  I  P T  S
      *    -----------------------------------
       UPDATE-RSVREC.
      *---------------
           MULTIPLY -1 BY CX-AMT.
           PERFORM OPEN-CX1-FILE.
           PERFORM UPDATE-FILES-CHECKTRANS.
           PERFORM CLOSE-CX1-FILE.
           MOVE "CLR" TO IO-TYPE.
           GO TO EXIT-UPDATE-ROUTINE.


      *    PERFORM UPDATE-TC-FILE.
      *    PERFORM UPDATE-CA-FILE.
      *    PERFORM UPDATE-DS-RESERVE.
      *    PERFORM UPDATE-TY-FILE.
      *    MOVE "CLR" TO IO-TYPE.
      *    GO TO EXIT-UPDATE-ROUTINE.

      *    -----------------------------------
      *       B    A    N    K
      *    -----------------------------------
       UPDATE-BANK.
      *---------------
           MOVE "UPDATE-BANK NOT VALID !" TO MESS.
           PERFORM SEND-MESS.
           MOVE "CLR" TO IO-TYPE.
           GO TO EXIT-UPDATE-ROUTINE.

       EXIT-UPDATE-ROUTINE.
           EXIT.

      *******************************************************
      * VERIFY PASSWORD SECTION
      *   --> ENTER PASSWORD IF MODIFYING/ADDING/DELETING
      *******************************************************
       VERIFY-PASSWORD SECTION.
           MOVE " " TO PASSWDW-STAT.
           MOVE "LONPI0" TO PASSWDW-PGM.
           MOVE "PLEASE ENTER THE PASSWORD," TO PASSWDW-PROMPT1.
           MOVE "CL/PASSWD" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH PASSWDW-BUF.
           CANCEL FORM-PROGX.


      *-  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
       REQUEST-CX1-FILE SECTION.

           MOVE "UPDATING CHECK REQUEST FILE............." TO MESS.
           PERFORM SEND-LEGEND.
      *    REF = SEQNO AIDS LONPM2 IN LOCATING CX-RECORD;
      *    CX-REF AND TT-CKWRITTEN ARE PASSED IN LOAN-KEY.

           MOVE TRANS-DATE TO CX-DATE.
           MOVE CX-REC TO SAVE-CX-REC.
           PERFORM OPEN-CX1-FILE.
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


      ****************************************
       UPDATE-FM1-FILE SECTION.
      ****************************************

           MOVE "UPDATING FORM REQUEST FILE............" TO MESS.
           PERFORM SEND-LEGEND.

           PERFORM OPEN-FM1-FILE.

           MOVE SPACES TO LPWSFM-EDIT-FM-INITKEY.

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

           MOVE CX-ZIP             TO FM-ZIP.
           MOVE SPACES             TO FM-PRINT.
           MOVE 0                  TO FM-STMT-COUNTER.
           MOVE TRANS-DATE         TO FM-TRDATE.
           MOVE EXT-CONAME-USER-ID TO FM-USERID.
           PERFORM WRITE-FM1-FILE.
           IF IO-FG NOT = 0
              GO TO IO-ERROR.

       EXIT-UPDATE-FM1.
           EXIT.

      *---------------------------------------------------------
       DISPLAY-DEALER-MASTER SECTION.
      *---------------------------------------------------------
           PERFORM OPEN-DL1-FILE.
           PERFORM READ-DL1-FILE.
           PERFORM CLOSE-DL1-FILE.
           IF IO-FG = 9
              MOVE "INVALID DEALER NO. !!" TO MESS
              PERFORM SEND-MESS
              MOVE 9 TO IO-FG
              GO TO EXIT-DISPLAY-DEALER-MASTER.

           MOVE DL-DLNO     TO D-0101.
           MOVE DL-NAME TO D-030.
           MOVE DL-ADR1 TO D-040.
           MOVE DL-ADR2 TO D-050.
           MOVE DL-CITY TO D-060.
           MOVE DL-STATE TO D-070.
           MOVE DL-ZIP TO D-080.
           PERFORM DISPLAY-NAME-ADDR.

           IF DL-TERMDATE NOT = 0
              IF DL-TERMDATE NOT > TRANS-DATE
                 MOVE " " TO MESS
                 MOVE DL-TERMDATE TO VDU-DATE-EDIT
                 STRING "DEALER TERMINATED ON " VDU-DATE-EDIT
                        ", POSTING IS NOT ALLOWED!"
                   DELIMITED BY SIZE INTO MESS
                 PERFORM SEND-MESS
                 MOVE 9 TO IO-FG.

       EXIT-DISPLAY-DEALER-MASTER.
           EXIT.

      *---------------------------------------------------------
       DISPLAY-BORROWER SECTION.
      *---------------------------------------------------------
           MOVE BW-SSNO TO NUM-BW.
           MOVE " " TO ALP-BW.
           IF NUM-BW NOT = 0
              MOVE "-" TO ALP-DASH1 ALP-DASH2
              MOVE NUM-BW123 TO ALP-BW123
              MOVE NUM-BW45 TO ALP-BW45
              MOVE NUM-BW6789 TO ALP-BW6789
              MOVE ALP-BW TO D-010.


           PERFORM OPEN-BW1-FILE.
           PERFORM READ-BW1-FILE.
           PERFORM CLOSE-BW1-FILE.
           IF IO-FG = 9
              MOVE CX-PAYEE TO INSPECT-W
              MOVE SPACES TO BW-ADR1 BW-ADR2 BW-CITY
              BW-STATE BW-ZIP
           ELSE
              PERFORM SETUP-BORROWER-NAME.
           MOVE INSPECT-W TO D-030.
           MOVE BW-ADR1 TO D-040.
           MOVE BW-ADR2 TO D-050.
           MOVE BW-CITY TO D-060.
           MOVE BW-STATE TO D-070.
           MOVE BW-ZIP TO D-080.
           PERFORM DISPLAY-NAME-ADDR.

      *---------------------------------------------------------
       SETUP-DEALER-BORROWER SECTION.
      *---------------------------------------------------------
           MOVE LN-SSNO(1) TO BW-SSNO.
           PERFORM OPEN-BW1-FILE.
           PERFORM READ-BW1-FILE.
           PERFORM CLOSE-BW1-FILE.
           IF IO-FG = 9
              MOVE "INVALID LOAN#" TO INSPECT-W
           ELSE
              PERFORM SETUP-BORROWER-NAME.

      *---------------------------------------------------------
       DISPLAY-SCREEN-BODY SECTION.
      *---------------------------------------------------------
           IF (DEALER)
              MOVE CX-DLNO      TO D-0101.
           MOVE CX-CLASS        TO D-020.
           MOVE CX-CLASS        TO LC-CODE.
           PERFORM GET-LC.
           MOVE LC-DESC         TO D-CLDESC.
           MOVE CX-ACCTNO       TO D-021.
           IF (DEALER)
              IF CX-ACCTNO NOT = 0
                 MOVE CX-ACCTNO TO LN-ACCTNO
                 PERFORM SETUP-DEALER-BORROWER
              ELSE
                 MOVE SPACES TO INSPECT-W
              END-IF
              MOVE INSPECT-W       TO D-BWNAME.

           MOVE CX-REASON       TO D-090.
           MOVE CX-AMT          TO D-100.
           MOVE CX-REF          TO D-110.

           MOVE CX-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO D-09A.
           IF CX-DATE = 0
              MOVE CX-DT-DATE TO DATE-YYYYMMDD
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
              MOVE DATE-MMDDYY TO D-09A.
           IF CX-BATCHID NOT = 0
              MOVE "BATCH ID:" TO D-09DESC
              MOVE CX-BATCHID TO D-09B.

           MOVE 0 TO SUB.
       NEXT-CXGL-DISP.
           ADD 1 TO SUB.
           IF CX-GLNO (SUB) NOT = 0
              MOVE CX-GLNO (SUB) TO W-GLNO
              MOVE W-GLBR TO D-GLBR(SUB)
              MOVE W-GLNUM TO D-GLNUM(SUB)
              MOVE CX-GLAMT (SUB) TO D-GLAMT (SUB).
           IF SUB < 5
              GO TO NEXT-CXGL-DISP.

           MOVE CX-FREE1 TO D-170.
           MOVE CX-FREE2 TO D-180.

           MOVE DISPLAY-BUF TO WR-BUF.
           MOVE DISPLAY-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      *---------------------------------------------------------
      * DT-SCAN NOT USED FOR DEALER RSV CHECK,
      *         TRY TO MATCH DT-REC WITH DEALER AND LOAN#
      *---------------------------------------------------------
       TRY-TO-FIND-DT SECTION.
           PERFORM CLEAR-DT-FILE.

           MOVE DT1-KEY TO HOLD-DT1-KEY.
           PERFORM OPEN-DT1-FILE.
           IF CX-ACCTNO = 0
              GO TO EXIT-TRY-FIND-DT.


           MOVE CX-BRNO     TO DT-BRNO
                              QDT1-WBEG-BRNO
                              QDT1-WEND-BRNO.
           MOVE CX-DLNO    TO DT-DLNO.
           MOVE DT-NUM     TO QDT1-WBEG-NUM
                              QDT1-WEND-NUM.
           MOVE CX-CLASS   TO DT-CLASS
                              QDT1-WBEG-CLASS
                              QDT1-WEND-CLASS.
           MOVE CX-ACCTNO  TO DT-ACCTNO
                              QDT1-WBEG-ACCTNO
                              QDT1-WEND-ACCTNO.
           MOVE 0          TO DT-SEQ
                              QDT1-WBEG-SEQ.
           MOVE ALL "9"    TO QDT1-WEND-SEQ.

           MOVE 19000101   TO DT-DATE SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD  TO QDT1-WBEG-DATE.
           MOVE EXT-JULIAN-END       TO SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD  TO QDT1-WEND-DATE.

           PERFORM START-DT1-FILE.
       FIND-DT-NEXT.
           PERFORM READ-DT1-FILE-NEXT.
           IF (IO-FG = 9) OR (DT-BRNO NOT = DT-PATH-OWNBR)
              GO TO EXIT-TRY-FIND-DT.

           IF DT-DLNO NOT = CX-DLNO
              MOVE 9 TO IO-FG
              GO TO EXIT-TRY-FIND-DT.
           IF DT-CLASS NOT = CX-CLASS
              MOVE 9 TO IO-FG
              GO TO EXIT-TRY-FIND-DT.
           IF DT-ACCTNO NOT = CX-ACCTNO
              MOVE 9 TO IO-FG
              GO TO EXIT-TRY-FIND-DT.
           IF DT-TRCD NOT = "NW"
              AND DT-TRCD NOT = "PU"
                 GO TO FIND-DT-NEXT.
           MOVE DT1-KEY TO HOLD-DT1-KEY.
       EXIT-TRY-FIND-DT.
           PERFORM CLOSE-DT1-FILE.

      *---------------------------------------------------------
       DISPLAY-DEALER-STATS SECTION.
      *---------------------------------------------------------
           PERFORM OPEN-DS1-FILE.

           MOVE TRANS-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-YYYYMM.
           MOVE DATE-YYYYMM TO DS-DATE-CCYYMM.

           MOVE CX-BRNO TO DS-BRANCH.
           MOVE CX-DLNO TO DS-DLNO.
           MOVE CX-CLASS TO DS-CLASS.
           PERFORM READ-DS1-FILE.
           PERFORM CLOSE-DS1-FILE.
           IF IO-FG = 9
      * GLB 08/07/01
      *       MOVE "UNABLE TO LOCATE DEALER STATS"
      *                                   TO MESS
      *       PERFORM SEND-MESS
      *       MOVE "DS STATS?" TO D-09DESC
      *       MOVE SPACES TO D-09B D-09C
              GO TO EXIT-DISPLAY-DEALER-STATS.
           IF CX-TYPE = "RR"
              MOVE "RSV PRIN"             TO D-09DESC
              MOVE DS-DLRVPRIN(CX-DLRVNO) TO D-09B
              MOVE SPACES                 TO D-09C.
           IF CX-TYPE = "RC"
              MOVE "RSV CHGS"             TO D-09DESC
              MOVE DS-DLRVCHGS(CX-DLRVNO) TO D-09B
              MOVE SPACES                 TO D-09C.
           IF CX-TYPE = "RP"
              MOVE "RSV HELD"             TO D-09DESC
              MOVE DS-DLPARVHELD          TO D-09B
              MOVE SPACES                 TO D-09C.
           IF CX-TYPE = "RD"
              MOVE "UNPAID CKS"           TO D-09DESC
              MOVE DS-UNPAIDCK            TO D-09B
              MOVE DS-UNPAID-MANUAL       TO D-09C.
           IF CX-TYPE = "DL"
              MOVE "UNPAID CKS"           TO D-09DESC
              MOVE DS-UNPAIDCK            TO D-09B
              MOVE DS-UNPAID-MANUAL       TO D-09C.
           IF CX-TYPE = "VX"
              MOVE "UNPAID CKS"           TO D-09DESC
              MOVE DS-UNPAIDCK            TO D-09B
              MOVE DS-UNPAID-MANUAL       TO D-09C.
       EXIT-DISPLAY-DEALER-STATS.
           EXIT.

      *---------------------------------------------------------
       HARD-COPY-YN SECTION.
      *---------------------------------------------------------
       HARD-COPY-QUESTION.
           MOVE "GENERATE HARD COPY (Y/N/A) ?       " TO MESS.
           PERFORM SEND-LEGEND.

           MOVE "E RA060SEL." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS NOT = "00"
              GO TO HARD-COPY-QUESTION.
           IF VDUBUF NOT = "     Y" AND
                      NOT = "     N"  AND
                      NOT = "     A"
              GO TO HARD-COPY-QUESTION.
           IF VDUBUF = "     A"
              MOVE "MC" TO IP-PROG
              MOVE "N" TO IP-AOK
              PERFORM RESET-SEL
              MOVE IP-PROG TO PROG-WK-IP
              MOVE PROG-WK TO FORM-NAM
              PERFORM XFER-FM-FIELDS
              PERFORM CLOSE-FILES
              CALL FORM-PROGX USING FORM-PATH PROG-BUF IP-BUF PRU-PATH
              CANCEL FORM-PROGX
              MOVE "Y" TO IP-AOK
              MOVE FD-PROG TO IP-PROG
              IF PRINT-PROG NOT = " "
                 MOVE PRINT-PROG TO IP-PROG
                 GO TO HARD-COPY-QUESTION
              ELSE
                 GO TO HARD-COPY-QUESTION.

      /********************************
      *    N E W   R E C O R D        *
      *********************************
       NEW-RECORD-SETUP SECTION.

           PERFORM CLEAR-CX-FILE.

           MOVE BRANCH TO CX-BRNO.
           MOVE "N" TO CX-DT-UNPAID.
           MOVE "I0" TO CX-PGM.
           MOVE 1 TO NEW-FLAG.
           MOVE 0 TO ELE.

      *---------------------------------------------------------
      *       COMMON UPDATE ROUTINES WITH ALL CHECK PRINTS
      *
       COPY "LIBLP/CHECKTRANS.CPY".
      *---------------------------------------------------------

       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBLP/LPMENU.CPY".
       COPY "LIBGB/GPENV.CPY".

       COPY "LIBGB/DATER.CPY".
       COPY "LIBLP/POSTDATE.CPY".

       COPY "LIBLP/LPFMCL.CPY".
       COPY "LIBLP/LPCXCL.CPY".
       COPY "LIBLP/LPDTCL.CPY".
       COPY "LIBLP/LPDSCL.CPY".
       COPY "LIBLP/LPTRCCL.CPY".
       COPY "LIBLP/LPTYCL.CPY".
       COPY "LIBLP/LPCKCL.CPY".

       COPY "LIBGB/CAPASSWD.CPY".

      *********************************************************
       FORM-RESET SECTION.
      *********************************************************
           DISPLAY LONPI-SCREEN UPON TCLP-MAIN-WINDOW-HANDLE.
           MOVE LONPI-FORM-FIELDS TO WR-BUF.
           MOVE LONPI-FORM-LIST   TO WR-LIST.
           PERFORM CALL-WRFORM.
           
       FORM-RESET-EXIT.
           EXIT.

      *********************************************************
       SETUP-LINK-INFO SECTION.
      *********************************************************
           IF VDU-FORM-NAME = "LPSMNU"
              MOVE LPSMNU-HELP--FILE TO HELP-LINK-FILE
              MOVE LPSMNU-HELP--DIR  TO HELP-LINK-DIR
           ELSE
              MOVE LONPI-HELP--FILE TO HELP-LINK-FILE
              MOVE LONPI-HELP--DIR TO HELP-LINK-DIR.

      *********************************************************
       VDU-CONVERT-FIELD SECTION.
      *********************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBLP/LONPI_EVA.CPY".
              COPY "LIBLP/LPSMNU_EVA.CPY".
              COPY "LIBLP/LPLNMU_EVA.CPY".
              COPY "LIBLP/LPCAMU_EVA.CPY".
              COPY "LIBLP/LPVDMU_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      /********************************
      *   M I S C   P A R A G R A P H *
      *********************************
       MISC-PARAGRAPHS SECTION.
       COPY "LIBGB/SYSTEM.CPY".
       COPY "LIBGB/GETENV.CPY".
       COPY "LIBGB/GETPR.CPY".
       ALIGNMENT-PRINT.
           MOVE "MC" TO IP-PROG.
           MOVE "N" TO IP-AOK.
           PERFORM RESET-SEL.
           MOVE IP-PROG TO PROG-WK-IP.
           MOVE PROG-WK TO FORM-NAM.
           PERFORM XFER-FM-FIELDS.
           PERFORM CLOSE-FILES.
           CALL FORM-PROGX USING FORM-PATH PROG-BUF IP-BUF PRU-PATH
           CANCEL FORM-PROGX.
           MOVE FD-PROG TO IP-PROG.
           IF PRINT-PROG NOT = " "
              MOVE PRINT-PROG TO IP-PROG.
           MOVE "Y" TO IP-AOK.

       SWITCH-CLASS.
           MOVE LN-CLASS TO CX-CLASS D-020.
           MOVE "CLASS MISMATCH, SWITCHING TO LOAN CLASS &&" TO MESS.
           INSPECT MESS REPLACING FIRST "&&" BY D-020.
           PERFORM SEND-MESS.
           MOVE 8 TO IO-FG.

       DETAIL-STATUS-CHECK.
           MOVE 0 TO IO-FG.
           IF DT-REVERSE  = "Y"
              MOVE "WARNING!  LOAN HAS BEEN DELETED "
                 TO MESS
              INSPECT MESS REPLACING FIRST "&" BY DT-STATUS
              PERFORM SEND-MESS.
           IF DT-TRCD NOT = "NW"
              AND DT-TRCD NOT = "PU"
                 MOVE "PLEASE SELECT VALID DETAIL LINE" TO MESS
                 PERFORM SEND-MESS
                 MOVE 9 TO IO-FG.
       GET-LN.
           PERFORM OPEN-LN1-FILE.
           PERFORM READ-LN1-FILE.
           PERFORM CLOSE-LN1-FILE.
       GET-FD1.
           MOVE IP-FMNO TO FD-FMNO.
           PERFORM OPEN-FD1-FILE.
           PERFORM READ-FD1-FILE.
           PERFORM CLOSE-FD1-FILE.
       GET-LC.
           PERFORM OPEN-LC1-FILE.
           PERFORM READ-LC1-FILE.
           PERFORM CLOSE-LC1-FILE.
           IF IO-FG = 9
              MOVE "---> NOT FOUND" TO LC-DESC.
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
       SETUP-BORROWER-NAME.
           MOVE BW-FNAME TO INSPECT-W.
           MOVE BW-LNAME TO ADD-ON.
           INSPECT INSPECT-W REPLACING FIRST "  " BY " +".
           INSPECT INSPECT-W REPLACING FIRST
              "+               " BY ADD-ON.
       SETUP-DT-FROM-CX.
           PERFORM CLEAR-DT-FILE.
           MOVE CX-DLNO     TO DT-DLNO.
           MOVE CX-CLASS    TO DT-CLASS.
           MOVE CX-ACCTNO   TO DT-ACCTNO.
           MOVE CX-BRNO     TO DT-BRNO.
           MOVE TRANS-DATE  TO DT-DATE.
       SETUP-CX-FROM-DT.
           MOVE DT-BRNO       TO CX-BRNO.
           MOVE DT-CLASS      TO CX-CLASS.
           MOVE DT-ACCTNO     TO CX-ACCTNO.
           MOVE CX-ACCTNO     TO CX-REF.
           MOVE DT-DLNO       TO CX-DLNO DL-DLNO.
           MOVE DT-DATE       TO CX-DT-DATE.
           MOVE DT-SEQ        TO CX-DT-SEQ.
           MOVE DT-PAID       TO CX-DT-PAID.
           MOVE DT-UNPAID     TO CX-DT-UNPAID.
      * IF A MANUAL CHECK FROM LOAN PROCESSING IS VOIDED,
      * DEALER STATS WILL HAVE ADDED TO DS-UNPAID-MANUAL.
      * REGARDLESS OF THE TYPE OF THE NEW CHECK (SYSTEM OR MANUAL)
      * THE ORIGINAL ENTRY TO UNPAID MUST BE OFFSET.
           IF DT-CHECK-VOID NOT = " "
                 IF DT-UNPAID = "N"
                    MOVE "M" TO CX-DT-UNPAID.

      * . . . . . F U T U R E . . . . . . ..
      *    MOVE DT-PROCEEDS     TO D-D-PROCEEDS(SUB)
      *    MOVE DT-PROCEEDS     TO HOLD-PROCEEDS
      *    IF DT-REVERSE = "Y"
      *         MOVE 0          TO HOLD-PROCEEDS
      *       END-IF
      *       COMPUTE HOLD-PAID = DT-RELEASED + DT-PAYMENTS
      *       MOVE HOLD-PAID      TO D-D-PAID(SUB)
      *       COMPUTE HOLD-DUE  = HOLD-PROCEEDS - HOLD-PAID
      * . . . . .. . . . . . . . . . . . . . . . .

           IF (DEALER-PROCEEDS)
              MOVE DT-PROCEEDS TO CX-AMT
              IF DT-REVERSE = "Y"
                 MOVE 0 TO CX-AMT
              END-IF
              COMPUTE CX-AMT  =  CX-AMT - DT-PAYMENTS.

      *---------------------------------------------------------
      * NEW TRANSACTION CODE, RECEIPT OF DEALER PROCEEDS;
      * DEAL VOIDED, CHECK ALREADY PROCESSED, DEALER WRITES NEW CHECK
      *---------------------------------------------------------
           IF CX-TYPE = "RD"
              MOVE DT-PROCEEDS TO CX-AMT
              IF DT-REVERSE = "Y"
                 MOVE 0 TO CX-AMT
              END-IF
              COMPUTE CX-AMT  =  -1 * (CX-AMT - DT-PAYMENTS).

           IF CX-TYPE = "DR"
              COMPUTE CX-AMT  =
                     (DT-DLRVPRIN(1) + DT-DLRVPRIN(2) + DT-DLRVPRIN(3) )
                     - DT-DLRVPRIN-PAY.
           IF CX-TYPE = "DC"
              COMPUTE CX-AMT =
                     (DT-DLRVCHGS(1) + DT-DLRVCHGS(2) + DT-DLRVCHGS(3) )
                     - DT-DLRVCHGS-PAY.
           IF CX-TYPE = "DP"
              COMPUTE CX-AMT =
                       DT-DLPARVHELD - DT-DLPARVHELD-PAY.

           MOVE SPACES TO MESS.
      * GLB 11/19/01
           STRING CX-TYPE ":CL-"
              D-020 "-" CX-ACCTNO " " D-09DESC
              DELIMITED BY SIZE INTO MESS.
           MOVE MESS TO CX-REASON.

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
           MOVE 99            TO L-NUMBER1.
           MOVE SAVE-CX-SEQNO TO L-NUMBER2.
           MOVE L-NUMBER      TO SAVE-CX-REF.
       SET-LOANKEY.
      *    THESE FIELDS ALLOW M2 TO LOCATE CX-REC.
      *  9 FOR LN-SEQNO IN LONPM2 TO KNOW 'CA'
      *    MOVE 9 TO L-SEQNO.
      *  USE PAYEE-SUB INSTEAD OF HARD CODED 9
           MOVE CX-REF    TO L-NUMBER.
           MOVE PAYEE-SUB TO L-PAYEE-SUB.
           MOVE L-ACCTNO  TO LOAN-KEY.
       NOT-FOUND.
           MOVE "NOT FOUND" TO MESS.
           PERFORM SEND-MESS.
       RESET-SCREEN.
           MOVE SPACES            TO WR-BUF.
           MOVE DISPLAY-HEAD-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
           MOVE SPACES       TO WR-BUF.
           MOVE DISPLAY-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
           MOVE SPACES                        TO WR-BUF.
           MOVE "TILL GLBAL REFPROMPT REFCD." TO WR-LIST.
           PERFORM CALL-WRFORM.
           PERFORM RESET-SEL.
       REFRESH-SCREEN.
           PERFORM DISPLAY-SCREEN-HEADER.
           MOVE DISPLAY-BUF TO WR-BUF.
           MOVE DISPLAY-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
           MOVE "S  S072GLBAL." TO VDU.
           MOVE 0 TO VDUNUM2.
           PERFORM SCREENIO.

       DISPLAY-SCREEN-HEADER.
           MOVE CX-PAID TO TRANS-TYPE.
           PERFORM VERIFY-TRANS-TYPE.

           MOVE CX-TYPE TO VDUBUF CHECK-PAYEE.
           PERFORM VERIFY-CHECK-PAYEE.

           MOVE CX-DLRVNO TO DLRVNO.

      * DONT REDISPLAY IF YOU ARE A DEBIT CARD. WE FORCED IT BACK TO A "C"
      * BUT WANT TO LEAVE THE "D" ON THE SCREEN, ONLY DISPLAY THE "DB" TYPE
           IF CX-PAID = "C" AND CX-TYPE = "DB"
              MOVE SPACES       TO DISPLAY-DEBIT-BUF
              MOVE "D"          TO DEBIT-TRANS-TYPE
              MOVE "DEBIT CARD" TO DEBIT-TRANS-DESC
              MOVE PAYEE-CODE   TO DEBIT-PAYEE-CODE
              MOVE PAYEE-DESC   TO DEBIT-PAYEE-DESC
              MOVE DISPLAY-DEBIT-BUF TO WR-BUF
           ELSE
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
           MOVE "SEL." TO MESS.
           PERFORM RESET-MESS.
       SEND-GLBAL.
           MOVE "S  S072GLBAL." TO VDU.
           PERFORM CHECK-GL-BALANCE.
           PERFORM SCREENIO.
       XFER-FM-FIELDS.
           MOVE FD-FMNO TO IP-FMNO.
      *    IP-PROG INITIALLY M2.
           IF FD-PRCONT = "Y"
              MOVE "1" TO CONT-PRINT
           ELSE
              MOVE "0" TO CONT-PRINT.

      **************************************************************
      *    USED TO SET CHFG'S AFTER CHECK IS SELECTED VIA SCAN
      **************************************************************
       SET-SCAN-CHFG-TABLE.
           IF CX-PGM = "B2"
              MOVE DIRECT-CHAN-TABLE TO CHFG-TABLE.
           IF P-CODE(PAYEE-SUB) = "LN"
              IF CX-PGM = "D0"
                 MOVE DIRECT-CHAN-TABLE TO CHFG-TABLE
              ELSE
      *  JTG 990706 ADDED LOGIC FOR REVOLVING DIRECT LOAN ADVANCES 'AV'
      *             VIA: LONPFM.C
                 IF CX-PGM = "F2" OR "FM"
                    MOVE TRANS-CHAN-TABLE TO CHFG-TABLE.


      *********************************
      *      F I L E   I / O          *
      *********************************
       IO-ROUTINE SECTION.
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBLP/LPTYGS_SQL.CPY".
       COPY "LIBLP/LPTRCGS_SQL.CPY".
       COPY "LIBLP/LPLNGS_SQL.CPY".
       COPY "LIBLP/LPLCGS_SQL.CPY".
       COPY "LIBLP/LPFMGS_SQL.CPY".
       COPY "LIBLP/LPFDGS_SQL.CPY".
       COPY "LIBLP/LPDTGS_SQL.CPY".
       COPY "LIBLP/LPDSGS_SQL.CPY".
       COPY "LIBLP/LPDLGS_SQL.CPY".
       COPY "LIBLP/LPCXGS_SQL.CPY".
       COPY "LIBLP/LPCKGS_SQL.CPY".
       COPY "LIBLP/LPCDGS_SQL.CPY".
       COPY "LIBLP/LPCAGS_SQL.CPY".
       COPY "LIBLP/LPBWGS_SQL.CPY".
       COPY "LIBGL/GLGIGS_SQL.CPY".
       COPY "LIBGB/GBGBGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBPRINT.CPY".
       COPY "LIBGB/GBGBRN.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGL/GLGI1RN.CPY".
       COPY "LIBLP/LPCD1RN.CPY".
       COPY "LIBLP/LPBW1RN.CPY".
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
       COPY "LIBLP/LPLC1RN.CPY".
       COPY "LIBLP/LPLN1RN.CPY".
       COPY "LIBLP/LPTRC1IN.CPY".
       COPY "LIBLP/LPTY1IN.CPY".
       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO ERRCD.
       END-ROUTINE SECTION.
           PERFORM PRU-OUT.
       END-PROG.
           PERFORM XFER-FM-FIELDS.
           PERFORM CLOSE-FILES.
       EXIT-PROG.
       STOP-RUN.
           EXIT PROGRAM.
