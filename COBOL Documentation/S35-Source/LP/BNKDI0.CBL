       IDENTIFICATION DIVISION.
       PROGRAM-ID.      BNKDI0.
       DATE-WRITTEN.    011585.
      *****************************************************
      *
      *  DESCRIPTION:   BANK DEPOSITS
      *
      * ALL AMOUNTS ENTERED POSITIVE
      *  GLNO'S ARE CREDITS
      *
      * LONPI0 = CASH RECEIPTS
      * DISBI0 = CASH DISBURSEMENTS  (WAS 1ONPI0)
      * CHCKI0 = CHECK WRITING       (WAS 2ONPI0)
      * VOIDI0 = VOID CHECKS         (WAS 3ONPI0)
      * BNKDI0 = BANK DEPOSITS       (WAS 4ONPI0)
      * BNKWI0 = BANK WITHDR         (WAS 5ONPI0)
      * BCHKI0 = BATCH CHECK WRITING (WAS 6ONPI0)
      *
      * REV:
      *  060595 BAH CHANGED THE FORM ALIGNED TO BE AN "A" IN THE
      *              GENERATE HARD COPY
      *  060895 BLL CHANGED TO USE NEW GL ENTRY ROUTINE
      *  061395 BLL CHANGED TO UPDATE TY-CKWRITTEN(7), NOT(6)
      *  062895 MJD ADDED COPY MEMBERS LIBLP/POSTDATE AND POSTDATEW.
      *             SAVE TC-USERID, POSTTIME, AND POSTDATE.
      *  073196 JTG ADDED ZERO CLEAR OF CX-DT-SEQ
      *  BLV 110397 MOVE IN BR-GL-TYPE-ALLOWED, BR-GL-ENTRY-DEFAULT TO
      *             ALLENT-BUF BEFORE GL DISTRIBUTION WINDOW
      *  MJD 112597 EXPANDED CX-REC TO 426 BYTES
      *  MJD 032098 ADDED GETENV(W) FOR USE IN LIBLP/POSTDATE
      *  JTG 000324 MODIFIED TO UPDATE UNEARNED STATISTICS, MERCURY #223
      *             NO CHANGE REQUIRED
      * GLB 000810 DLR:SUBSTITUTE COPY MEMBER LPCXCL TO CLEAR CX-RECORD
      * GLB 030325 WORLD PR# 320;RECOMPILE DUE TO CHECKTRANSW/CHECKTRANS
      * BAH 040112 STOPPED THE SCREEN FROM RESETTING TO ALL SPACES INSTEAD
      *            OF THE UNDERLINES , JUST ME !
      * BLF 060719 ADD PA-FILE & GPENV FOR UPDATE OF YTD PAID AMT IN
      *            PAFILE IN CHECKTRANS, DEVELOPMENT PR# 2633
      *  CS 120214 ADDED NEW PAYEE TYPE OF 'ID'[INTERIM DEPOSIT]. WORLD WANTED
      *            CASH DRAWER UPDATED WITHOUT FORCING THE USER TO DO A BK &
      *            THEN A CASH DISBURSEMENT. ADDED IF GP-I-AM-WORLD, FORCES 'BK'
      *            TO TRANS-TYPE & ALLOW AN INPUT OF 'BK' OR 'ID' TO PAYEE
      *            TYPE. IF 'BK' ACTS AS IT DOES TODAY & ALLOCATION WINDOW IS
      *            DISPLAYED. IF 'ID' PROMPTS FOR THE CASH DRAWER NUMBER TO
      *            BE UPDATED & FORCES THE BR-GLONHAND TO THE FIRST LEVEL OF
      *            GL ENTRY, NO ADDITIONAL ENTRIES ARE PERMITTED. IF YOU ARE
      *            NOT WORLD, PAYEE TYPE IS FORCED TO 'BK' AND NO ENTRY OF
      *            CASH DRAWER IS ALLOWED.  WORLD PR# 790
      *  CS 120222 WHEN PRINTING HARD COPY CREATED CK,CX & DIDNT DELETE EITHER,
      *            MADE CHANGES TO CHECKTRANS FOR NEW 'ID' & 'IW' PR#790
      *  CS 120620 ADD PASSWD TO USE 'BK' PAYEE IF GP-I-AM-WORLD, SET DEFAULT
      *            PAYEE TO 'ID'   WORLD PR#810
      *            "ID" & "IW" ONLY WORK WITH CHECK PGMS LONPM2 & LONP3T
      * BAH 140502 RENAMED FROM 4ONPI0, VERYANT PROGRAMS CANT START NUMERIC
      * BAH 140814 ADDED TEST FOR LPSMNU IN HELP
      *  CS 160201 REMOVED GIFILE, NOT USED, SAVE VERSION BNKDI0.160201
      * BLM 160511 REMOVE LPSETDS & LPSETDSW
      * BAH 170508 ACTIVATE 8 DIGIT DATES, PD0006
      * BAH 190313 MOVED EDIT-FM-INITKEY TO LPWSFM-EDIT-FM-INITKEY 
      * BAH 20190614 ADDED CLEAR-FM-FILE (LPFMCL) AND SET FM-USERID
      * BAH 2022.0314 REMOVED EXT-SPANISH-FORMS AND ALL MX FORMS
      **************************************************************************
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
           "@(#)LP/BNKDI0.CBL S35 04/30/24-13:19:41 >".
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
       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBGB/PASSWDW.CPY".
       01  DLRVNO3-SEQ            PIC 9  VALUE 0.
       01  BK-SEQ-DEPOSIT         PIC 9  VALUE 1.
       COPY "LIBGB/GPENVW.CPY".
       COPY "LIBGB/SYSTEMW.CPY".

       COPY "LIBGL/ALLENTW.CPY".

       COPY "LIBLP/LPDSCLW.CPY".
       COPY "LIBLP/LPMENUW.CPY".
       COPY "LIBLP/LPSETTYW.CPY".
       COPY "LIBLP/POSTDATEW.CPY".

       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBLP/BNKDI_DEF.CPY".
       COPY "LIBLP/LPSMNU_DEF.CPY".
       COPY "LIBLP/LPLNMU_DEF.CPY".
       COPY "LIBLP/LPCAMU_DEF.CPY".
       COPY "LIBLP/LPVDMU_DEF.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBLP/BNKDI_WKS.CPY".
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
       COPY "LIBLP/BNKDI_SCN.CPY".
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
           PERFORM CLOSE-CX1-FILE.
           PERFORM CLOSE-CK1-FILE.
           PERFORM CLOSE-CA1-FILE.
           PERFORM CLOSE-GB-FILE.
           PERFORM CLOSE-BR-FILE.
           PERFORM CLOSE-PR-FILE.
       COPY "LIBGB/DECLAR2.CPY".

      ******************************************
       MAIN-PROGRAM SECTION.
      ******************************************

           PERFORM SQL-CONNECT.

           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "BNKDI" TO VDU-FORM-NAME.

           PERFORM FORM-RESET.

           PERFORM GET-GPENV.

           PERFORM GET-GLOBAL.

           PERFORM GET-BRANCH.

           MOVE 0 TO Q-POS C-POS.
           MOVE BNKDI-PRU-POS    TO P-POS.
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
              MOVE 5 TO IP-FMNO .

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
              PERFORM PRU-OUT
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
      *    E N T E R   K E Y S           *
      ************************************

       ENTER-KEYS SECTION.
       RESEND-PRU-FMNO.
      *    PRU AND FMNO NOT REENTERED UPON F1 RESET.
           PERFORM SEND-PRU-FORM.
      *---------------------------------------------------------
      * ENTER TRANS TYPE; S=SYSTEM,   M=MANUAL
      *                   V=VOID,     R=RSV REC
      *                   C=CASH REC,*B=BANK           * = VALID
      *---------------------------------------------------------
       ENTER-TRANS-TYPE.
           MOVE "N" TO CXFILE-SCAN-FLAG.
           MOVE 0 TO NEW-FLAG CHECK-NO.
           MOVE SPACES TO IO-TYPE.

           PERFORM NEW-RECORD-SETUP.

           MOVE CX-PAID          TO HOLD-TRANS-TYPE.
           MOVE CX-TYPE          TO HOLD-CHECK-PAYEE.
           MOVE CX-DLRVNO        TO HOLD-DLRVNO.

       SEND-TRANS-TYPE.
           MOVE "B" TO CX-PAID TRANS-TYPE HOLD-TRANS-TYPE.
           MOVE "D" TO CX-DEPOSIT-WITHDR.
           PERFORM VERIFY-TRANS-TYPE.
           IF TYPE-SUB > TYPE-MAX
              MOVE "INVALID CX-PAID 'B'" TO MESS
              PERFORM SEND-MESS
              GO TO END-PROG.

           MOVE "ID" TO CX-TYPE CHECK-PAYEE HOLD-CHECK-PAYEE.

           MOVE WORLD-BANK-DEPOSIT  TO MESS
           PERFORM SEND-LEGEND.

           PERFORM DISPLAY-SCREEN-HEADER.

       ENTER-PAYEE-TYPE.

      *IF WORLD ALLOW THE ENTRY OF A 'BK' OR 'ID'
           MOVE "E  A020K020." TO VDU.

           PERFORM SCREENIO.
           IF F7-KEY
              GO TO MENU-KEY.
           IF NOT(VDUBUF = "BK" OR "ID")
              GO TO ENTER-PAYEE-TYPE.
           MOVE VDUBUF TO CX-TYPE CHECK-PAYEE HOLD-CHECK-PAYEE.
           IF CX-TYPE  = "BK"
              MOVE BK-SEQ-DEPOSIT TO PASSWDW-SEQ
              MOVE "PLEASE ENTER THE PASSWORD," TO PASSWDW-PROMPT1
              MOVE "TO USE 'BK' PAYEE"          TO PASSWDW-PROMPT2
              PERFORM VERIFY-PASSWORD
              IF PASSWDW-STAT NOT  = "OK"
                 GO TO ENTER-PAYEE-TYPE.

       VERIFY-ENTRY.
           PERFORM VERIFY-CHECK-PAYEE.
           IF PAYEE-SUB > PAYEE-MAX
              MOVE "INVALID CX-TYPE 'BK'" TO MESS
              PERFORM SEND-MESS
              GO TO END-PROG.

           PERFORM DISPLAY-SCREEN-HEADER.

       ENTER-TILLNO.
           IF NOT (INTERDEP)
               GO TO EXIT-ENTER-KEYS.

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
      * - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF VDUSTATUS = "1>"
              GO TO MENU-KEY.
      *    IF VDUSTATUS = "1U"
      *       IF TRANS-TYPE = "C"
      *          GO TO ENTER-TRANS-TYPE
      *       ELSE
      *          GO TO ENTER-PAYEE-TYPE.
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

      *VERIFY DRAWER OPEN FOR INTERIM DEPOSIT
      **************************************
       VERIFY-TILL SECTION.
      **************************************
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



      /    ************************************************
      *    *   ENTRY-MODULE
      *    *   NEW-FLAG = 0 - MODIFY EXISTING RECORD
      *    *              1 - INITIAL NEW RECORD ENTRY
      *    *              2 - MODIFY NEW RECORD
      *    ************************************************
       ENTRY-MODULE SECTION.
       BEGIN-ENTRY.
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

           PERFORM CHECK-GL-BALANCE.
           IF BAL-FG  = 9
              MOVE "CASH NOT = G/L DISTRIBUTION" TO MESS
              PERFORM SEND-MESS
              GO TO MODIFY-ROUTINE.

      *---------------------------------------------------------
      *    U P D A T E  /  P R I N T    H A R D    C O P Y
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
           IF ELE = 12 OR 13 OR 14 OR 15 OR 16
              PERFORM ENTER-GL
              GO TO CREATE-ROUTINE.
           MOVE SPEC(ELE) TO VDU.
           MOVE ELE TO VDUROW.
           MOVE 0 TO VDUCOL.
           MOVE "." TO VDUDECIMAL.
           PERFORM SCREENIO.
       TEST-F1.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
       TEST-F4.
           IF VDUSTATUS NOT = "1U"
              GO TO TEST-F5.
           IF NEW-FLAG = 1
              MOVE -1 TO ONE
              GO TO CREATE-ROUTINE.
       TEST-F5.
           IF VDUSTATUS NOT = "1D"
              GO TO TEST-F6.
           MOVE 1 TO ONE.
           GO TO GOTO-FIELD.
       TEST-F6.
           IF VDUSTATUS NOT = "1<"
              GO TO TEST-F7.
           IF ELE > 11 AND ELE < 17
              GO TO GOTO-FIELD.
       TEST-F7.
           IF VDUSTATUS NOT = "1>"
              GO TO TEST-00.
           PERFORM PRU-OUT.
           DESTROY ALL CONTROLS.
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
           GO TO CREATE-ROUTINE.
       F-02.
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
           MOVE VDUBUF TO CX-REASON.
           GO TO CREATE-ROUTINE.
       F-10.
           MOVE VDUNUM2 TO CX-AMT.
      *C   IF VDUNUM2 < 0
           IF VDUNUM2 NOT >  0
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       F-11.
           MOVE VDUNUM0 TO CX-REF.
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
           MOVE "S  A000K022." TO VDU.
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
      *----
           IF (INTERDEP)
              IF CX-TILLNO = 0
                 MOVE "INTERIM DEPOSIT" TO PAYEE-DESC.


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
      *   E N T E R   G/L #   A M T   *
      *********************************
       ENTER-GL SECTION.
       BEGIN-ENTER-GL.
           MOVE CX-AMT TO ALLENT-DISBURSE.
           IF CXFILE-SCAN-FLAG = "Y"
              MOVE 0 TO ALLENT-DISBURSE.
           IF (DEALER-RSV) OR TRANS-TYPE = "R"
              MOVE 0 TO ALLENT-DISBURSE.
           MOVE BRANCH TO ALLENT-BRNO.
           MOVE BR-OTHER-BRANCH-GL TO ALLENT-OTHER-BR-FG.
           MOVE BR-GL-TYPE-ALLOWED TO ALLENT-GL-TYPE-ALLOWED.
           MOVE BR-GL-ENTRY-DEFAULT TO ALLENT-GL-ENTRY-DEFAULT.
           MOVE 5 TO ALLENT-NO-OF-ACCTS.

           IF (INTERDEP)
              MOVE 0 TO ALLENT-BR-ACCOUNT(1) ALLENT-AMT(1)
                        ALLENT-BR-ACCOUNT(2) ALLENT-AMT(2)
                        ALLENT-BR-ACCOUNT(3) ALLENT-AMT(3)
                        ALLENT-BR-ACCOUNT(4) ALLENT-AMT(4)
                        ALLENT-BR-ACCOUNT(5) ALLENT-AMT(5)
              MOVE BR-GLONHAND TO ALLENT-BR-ACCOUNT(1)
              MOVE CX-AMT TO ALLENT-AMT(1)
              GO TO START-GL-LOOP.

           MOVE "GL/ALLENT" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH ALLENT-BUF.
           CANCEL FORM-PROGX.
       START-GL-LOOP.
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
           MOVE 0 TO BAL-FG VDUNUM2.
           IF CXFILE-SCAN-FLAG = "Y"
                 MOVE CX-AMT TO VDUNUM2.
      *  TESTING DEALER CHECK
      *       OR DEALER RESERVE CASH RECEIPTS (SEE LONPI0)
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

           GO TO
              UPDATE-MANUAL UPDATE-SYSTEM
              UPDATE-VOID   UPDATE-RSVREC
              UPDATE-CASH   UPDATE-BANK
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
           MOVE "UPDATE-CASH RECEIPTS NOT VALID !" TO MESS.
           PERFORM SEND-MESS.
           MOVE "CLR" TO IO-TYPE.
           GO TO EXIT-UPDATE-ROUTINE.

      *    -----------------------------------
      *       R  S  V     R E  C  E  I  P T  S
      *    -----------------------------------
       UPDATE-RSVREC.
      *---------------
           MOVE "UPDATE-DEALER RSV RECEIPTS NOT VALID !" TO MESS.
           PERFORM SEND-MESS.
           MOVE "CLR" TO IO-TYPE.
           GO TO EXIT-UPDATE-ROUTINE.

      *    -----------------------------------
      *       B    A    N    K
      *    -----------------------------------
       UPDATE-BANK.
      *---------------
           IF (INTERDEP)
               PERFORM UPDATE-CA-FILE.
           MULTIPLY -1 BY CX-AMT.
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 5
             MULTIPLY -1 BY CX-GLAMT(SUB)
           END-PERFORM.


           PERFORM UPDATE-TC-FILE.

      *    MOVE 7 TO PAYEE-SUB.
      *  PAYEE-SUB MUST BE 7 IN UPDATE-TT1 FOR CKWRITTEN(7)-INT DEP
      *  GLB 09/00 NOW TESTS CHECK-PAYEE INSTEAD
           PERFORM UPDATE-TY-FILE.


           PERFORM HARD-COPY-YN.
           IF VDUBUF = "     N"
              MOVE "CLR" TO IO-TYPE
              GO TO EXIT-UPDATE-ROUTINE.

           PERFORM REQUEST-CX1-FILE.
           PERFORM UPDATE-FM1-FILE.


      *    MOVE 10 TO PAYEE-SUB.
      *  PAYEE-SUB = 10 FOR LN-SEQNO IN LONPM2 TO KNOW 'BK'
      *  GLB 09/00 DONT NEED TO FORCE PAYEE SUB ANYMORE
           PERFORM SET-LOANKEY.
           MOVE "END" TO IO-TYPE.

       EXIT-UPDATE-ROUTINE.
           EXIT.


      *******************************************************
      * VERIFY PASSWORD SECTION
      *   --> ENTER PASSWORD IF MODIFYING/ADDING/DELETING
      *******************************************************
       VERIFY-PASSWORD SECTION.
           MOVE " " TO PASSWDW-STAT.
           MOVE "BNKDI0" TO PASSWDW-PGM.
           MOVE "CL/PASSWD" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH PASSWDW-BUF.
           CANCEL FORM-PROGX.

      *****************************************************
       REQUEST-CX1-FILE SECTION.
      *****************************************************

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


      **************************************
       UPDATE-FM1-FILE SECTION.
      **************************************

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

           MOVE CX-ZIP                 TO FM-ZIP.
           MOVE SPACES                 TO FM-PRINT.
           MOVE 0                      TO FM-STMT-COUNTER.
           MOVE TRANS-DATE             TO FM-TRDATE.
           MOVE EXT-CONAME-USER-ID     TO FM-USERID.

           PERFORM WRITE-FM1-FILE.
           IF IO-FG NOT = 0
              GO TO IO-ERROR.

       EXIT-UPDATE-FM1.
           EXIT.

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


      **************************************
       HARD-COPY-YN SECTION.
      **************************************
       HARD-COPY-QUESTION.
           MOVE "GENERATE HARD COPY (Y/N/A) ?       " TO MESS.
           PERFORM SEND-LEGEND.

           MOVE "E RA060SEL." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS NOT = "00"
              GO TO HARD-COPY-QUESTION.
           IF VDUBUF NOT = "     Y" AND
                      NOT = "     N" AND
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

      *---------------------------------------------------------
      *       COMMON UPDATE ROUTINES WITH ALL CHECK PRINTS
      *
       COPY "LIBLP/CHECKTRANS.CPY".
      *---------------------------------------------------------

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
       COPY "LIBGB/CAPASSWD.CPY".

      ************************************************
       FORM-RESET SECTION.
      ************************************************
           DISPLAY BNKDI-SCREEN UPON TCLP-MAIN-WINDOW-HANDLE.
           MOVE BNKDI-FORM-FIELDS TO WR-BUF.
           MOVE BNKDI-FORM-LIST   TO WR-LIST.
           PERFORM CALL-WRFORM.
           

      ************************************************
       SETUP-LINK-INFO SECTION.
      ************************************************
           IF VDU-FORM-NAME = "LPSMNU"
              MOVE LPSMNU-HELP--FILE TO HELP-LINK-FILE
              MOVE LPSMNU-HELP--DIR  TO HELP-LINK-DIR
           ELSE
              MOVE BNKDI-HELP--FILE TO HELP-LINK-FILE
              MOVE BNKDI-HELP--DIR TO HELP-LINK-DIR.
     

      ************************************************
       VDU-CONVERT-FIELD SECTION.
      ************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBLP/BNKDI_EVA.CPY".
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
       COPY "LIBGB/GETPR.CPY".
       COPY "LIBGB/SYSTEM.CPY".
       COPY "LIBGB/GETENV.CPY".
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
           MOVE SPACES TO DISPLAY-BUF.
           MOVE DISPLAY-BUF TO WR-BUF.
           MOVE DISPLAY-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      *    MOVE SPACES TO WR-BUF.
      *    MOVE "FMKEY K030." TO WR-LIST.
      *    PERFORM CALL-WRFORM.

       DISPLAY-SCREEN-HEADER.
           MOVE CX-PAID TO TRANS-TYPE.
           PERFORM VERIFY-TRANS-TYPE.

           MOVE CX-TYPE TO CHECK-PAYEE.
           PERFORM VERIFY-CHECK-PAYEE.

           MOVE CX-DLRVNO TO DLRVNO.

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
       ALIGNMENT-PRINT.
           MOVE "MC" TO IP-PROG.
           MOVE "N" TO IP-AOK.
           PERFORM RESET-SEL.
           MOVE IP-PROG TO PROG-WK-IP.
           MOVE PROG-WK TO FORM-NAM.
           PERFORM XFER-FM-FIELDS.
           PERFORM CLOSE-FILES.
           CALL FORM-PROGX USING FORM-PATH PROG-BUF IP-BUF PRU-PATH.
           CANCEL FORM-PROGX.
           MOVE FD-PROG TO IP-PROG.
           IF PRINT-PROG NOT = " "
              MOVE PRINT-PROG TO IP-PROG.
           MOVE "Y" TO IP-AOK.

      *********************************
       IO-ROUTINE SECTION.
      *********************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBLP/LPTYGS_SQL.CPY".
       COPY "LIBLP/LPTRCGS_SQL.CPY".
       COPY "LIBLP/LPLNGS_SQL.CPY".
       COPY "LIBLP/LPLCGS_SQL.CPY".
       COPY "LIBLP/LPFMGS_SQL.CPY".
       COPY "LIBLP/LPFDGS_SQL.CPY".
       COPY "LIBLP/LPDTGS_SQL.CPY".
       COPY "LIBLP/LPDSGS_SQL.CPY".
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

      **************************************************
       END-ROUTINE SECTION.
      **************************************************
           PERFORM PRU-OUT.
       END-PROG.
           PERFORM XFER-FM-FIELDS.
           PERFORM CLOSE-FILES.
       EXIT-PROG.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY BNKDI-SCREEN.
       STOP-RUN.
           EXIT PROGRAM.
