       IDENTIFICATION DIVISION.
       PROGRAM-ID.      CHCKI0.
       DATE-WRITTEN.    011585.
      *****************************************************************
      *                 (LP)
      *  DESCRIPTION:   CHECK WRITING
      *                MANUAL OR SYSTEM 'MS' CHECKS
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
      *  052295 BLL MATCH DT-SEQ TO CX-DT-SEQ FOR MULTIPLE CKS
      *  060595 BAH CHANGED ALIGNMENT PRINT TO BE F3-ALIGN INSTEAD
      *             OF ASKING EACH TIME
      *  060795 BLL ADD NEW GL ENTRY ROUTINE

      *         BECAUSE MERCURY WILL BE WRITING TO THEMSELVES
      *  MJD 062795 ADDED COPY MEMBERS LIBLP/POSTDATE AND POSTDATEW.
      *              SAVE TC-POSTDATE, POSTTIME, AND USERID.
      *  MJD 091595 ADDED DS-DATE LOGIC.
      *  MJD 100395 ADDED PASSWORD REQUIREMENT TO ACCESS RESERVE 3.
      *  MJD 011296 ADDED CHECK OF BR-NO-OUT-DLR-RSV FOR "DR", "DC","DP"
      *             TRANSACTIONS.
      *  MJD 020896 ADDED UPDATE OF TY DEALER RESERVES (NET GAIN/LOSS)
      *  BLL 042996 CHECK IF ENOUGH AFP FOR CHECK IF DLRV3,  & SUBTRACT
      *             CHECK AMT FROM DS-AFPDUEDL-MEMO.
      *  JTG 082996 CHANGED DEALER CHECKS TO NOT ALLOW OVERRIDE ON AMOUNT
      *  BLV 103096 FIXED GL DISPLAY AFTER CXSCAN
      *  CLS 122396 ADDED TEST OF GI FOR RESERVE CHECKS
      *  JTG 011797 CORRECTED BUG WITH RESERVE SYSTEM CHECKS
      *             NOT STOPPING ON CHECK AMOUNT
      *  BAH 011797 DONT DISPLAY AFP DS MEMO MESSAGE UNLESS IT IS AN AFP
      *             DEALER...
      *  JTG 011897 MODIFIED TO ALLOW MANUAL INPUTED "DL" CHECKS
      *  BAH 050997 DONT ALLOW UPDATE OF IN-RESCISSION CHECKS
      *  BAH 051697 ADDED ANOTHER READ OF CK1 RIGHT BEFORE THE UPDATE
      *             TO CHECK FOR DUP'S AGAIN, BECAUSE 2 PEOPLE CAN ENTER
      *             THE CHECK # AT SAME TIME, AND BOTH UPDATE, CAUSING
      *             2 TRC RECS AND ONLY 1 CK REC....
      *  JTG 060497 STOPPED MANUAL 'DL' CHECKS
      *  BLV 103197 MOVE IN BR-GL-TYPE-ALLOWED, BR-GL-ENTRY-DEFAULT TO
      *             ALLENT-BUF BEFORE GL DISTRIBUTION WINDOW
      *  MJD 112597 EXPANDED CX-REC TO 426 BYTES
      *  MJD 032098 ADDED GETENV(W) FOR USE IN LIBLP/POSTDATE
      *  KEC 120298 FIXED DISPLAY OF DESCRIPTION TO "K011" USING THE
      *              TYPE-TABLE (TYPE/DESCRIPTION). WAS CUTTING OFF THE
      *              FIRST BYTE OF DESCRIPTION.
      *  JTG 990706 ADDED LOGIC FOR REVOLVING DIRECT LOAN ADVANCES 'AV'
      *  JTG 000324 MODIFIED TO UPDATE UNEARNED STATISTICS, MERCURY #223
      *             NO CHANGE REQUIRED
      *  GLB 000511 ADDED LOGIC LIKE IN LONPI0 CASH RECEIPTS TO LOOK FOR
      *             A VALID LOAN# IN REF AND PLUG IT INTO THE DT-ACCTNO
      *             FOR DEALER CHECKS
      *  BAH 010306 WHY NOT DISPLAY ADDRESS,CITY,STATE,ZIP WHEN SCANNING
      *             CX RECORDS ??? GREAT IDEA !! COLTN, PL# 318
      *  BAH 010306 ALLOW CHANGE TO DEALER CITY,STATE,ZIP WHEN SCANNING
      *             ADDED SALES-CHAN-TABLE FOR "B2", COLTN PL# 318
      *  BAH 010925 "DL" SCAN SYSTEM CHECKS NOT WORKING. FIXED.
      *             WASNT DISPLAYING PAYEE INFO CORRECT, WASNT PUTTING
      *             CURSER IN RIGHT SPOT, ETC...
      *  GLB 010927 REWORK VALIDATING LOAN MASTERS, DT-RECORDS.
      *             MULTIPLE CHECKS WITH VOIDS CAUSING PROBLEMS
      * GLB 020128 NOW NEED TO ENTER WI/DTSCAN WITH A 0 DATE, NOT TRANS-DATE
      * BAH 020130 REMOVED THE PRU-OUT IN EXIT-PROG, CAUSING "CANNOT ENABLE
      *            PRINTER" ERROR MESSAGE WHEN F7 EXIT WHEN USING A
      *            SYSTEM PRINTER
      * BAH 020220 NON-NUMERIC AT 3A1B, CHANGED LIBLP/CHECKTRANS TO NOT TRY
      *            TO DELETE-CX1-FILE IF MANUAL CHECK, IF SCANNED IN IT WILL
      *            GET DELETED IN HERE, PLUS THE LN-ACCTNO IS NEVER SET, THAT
      *            IS WHAT IS CAUSING THE NON-NUMERIC
      * BAH 020305 WAS ALLOWING YOU TO CHANGE THE $$$$ AMOUNT OF THE CHECK
      *            IF YOU WERE SCANNING A BATCH CHECK, THEN TRYING TO SAY
      *            MANUAL, IT ACTED AS IF YOU WERE BUILDING A MANUAL CHECK
      *            FROM SCRATCH, AND ALLOWED YOU TO CHANGE ANYTHING !!
      * GLB 030325 WORLD PR# 320; ADD NEW CHECK PAYEE TYPE "DD" TO
      *            AUTOMATICALLY INCREASE CASH DRAWER AND DEFAULT
      *            G/L OFFSET TO CASH ON HAND
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
      * BAH 050816 DISPLAY A WARNING MESSAGE IF 25TH BYTE OF ADDRESS IS
      *            FILLED IN, NEWPORT PR# 2589
      *  CS 060616 PROCEEDS FOR SERVICE ACCOUNT NOT FLAGGED AS SERVICE ACCOUNT
      *            WHEN SCAN FOR F6-PAYEE... ENDS UP UPDATING DT-TYPE AS 'P'
      *            ADDED TEST IF CX-SCAN-FLAGE = "Y" GET THE LC-SERVACCT &
      *            MOVE TO CX-SERVACCT  PR#2503 SECURITY AUTO
      *  CS 060616 ADD CHECKS FOR GP-I-AM-SECURITY TO RESTRICT ENTRY OF RESERVE
      *            BASED ON DEALER/CLASS COMBO SECURITY AUTO #2503
      *  CS 060616 VALADATE EVENT-CODE PR#2503 SECURITY
      * BLF 060712 ADD PAFILE (PAYEE FILE) & ENTRY/SCAN OF PAYEE NUMBER FOR
      *            MISC CHECK IF GLOBAL PARAMETER IS TURNED ON, DEVELOPEMENT
      *            PR# 2633
      *  CS 060713 WAS ALLOWING OPERATOR TO ENTER RECEIPT CODES (RR,RC,RP,RD)
      *            PR# 544 CINDY&JUDY
      *  CS 060731 FIXED GP-I-AM-SECURITY TEST PR#2503
      * BLF 060802 ADD ABILITY TO CREATE PAYEE BY CALLING PAMAN1,
      *            DEVELOPMENT PR#2633
      * BAH 060803 DONT ALLOW "CA" AND "BK" PAYEE TYPES !!!
      * BAH 080513 ADDED INTERRUPT-FG FROM F10 CL/WIMENU FOR RIVERSIDE WISC
      *            ONLY FOR THEM UNTIL TESTED GOOD... RIVERSIDE PR# 3192
      * BAH 081016 CHANGED WIMENU-SELECTION TO A "2" SO IT WOULD HAVE ITS
      *            OWN DISPLAY F10 WINDOW WITHOUT CHECK WRITING IN IT
      *  CS 090728 IF GP-I-AM-SECURITY DONT ALLOW A DEALER RESERVE CHECK IF
      *            DEALER PLAN (DL-BR) = 4   SECURITY AUTO PR# 3420
      * BAH 130122 REMOVE RESERVE RESTRICTIONS FOR SECURITY AUTO PR#4199
      * BAH 140502 RENAMED FROM CHCKI0, VERYANT PROGRAMS CANT START NUMERIC
      * BAH 140814 ADDED TEST FOR LPSMNU IN HELP
      *  CS 141208 ADD MOVE OF EXT-CONAME-SYSDATE TO FORM-RESET,TRANSDATE WAS
      *            SPACES.
      *  CS 150204 IF BR-AUTO-CHECK = "Y", WHEN 'U'PDATE IS DONE, PRIOR TO
      *            INSERT NEXT CHECK MESSAGE, READ NEXT CHECK FROM BRANCH AND
      *            COMPARE TO CX, IF DIFFERENT ISSUE MESSAGE PR#4692
      * BAH 150713 CHANGED DIRECT-CHAN-TABLE IN CHECKTRANSW TO ALLOW YOU
      *            TO CHANGE FIELDS 5 THROUGH 8, ADR2 - ZIP, REGMGM PR# 4786
      * KEC 2016.0323 CHANGED CDSCAN-WINDOW-BUF TO HAVE MATCHING CD1-KEY
      *               LAYOUTS (FOR EACH CODE TYPE; CD-TYPE).
      *               CDSCAN-CD1-KEY IS THE NEW (PRIMARY) WITH REDEFINES OF
      *               CDSCAN-CD-??-KEY (NEW).
      * BLM 160511 FIX DS-DATE LOGIC FOR NEW FORMAT, PD0003
      * BAH 160713 DO NOT SWITCH TO A SERVICE CLASS AFTER "CLASS MISMATCH"
      *            ERROR MESSAGE FOR DLR RESERVE CHECKS, PEP PL#944
      * BAH 160921 PASSWORD DISPLAYING EXISTING CHECK INFO, COMM LENDER 5174
      * KEC 2017.0105 CHANGED DS-DATE TO DS-DATE-CCYYMM FOR CONSISTENCY
      *               WITH THE DATE FIELDS IN BY & UD FILE.
      * BAH 170511 ACTIVATE 8 DIGIT DATES, PD0006
      * BAH 181009 GLOBAL CXFILE
      * BAH 181114 GLOBAL FMFILE
      * BAH 190122 SPLIT CDFILE
      * BAH 190122 SPLIT CDFILE
      * AMM 190207 CHANGED PAMAN1-PA-NO TO PAMAN1-PA-NUMBER
      * BAH 190312 MOVED EDIT-FM-INITKEY TO LPWSFM-EDIT-FM-INITKEY 
      * BAH 20190614 ADDED CLEAR-FM-FILE (LPFMCL) AND SET FM-USERID
      * BAH 2022.0314 REMOVED EXT-SPANISH-FORMS AND ALL MX FORMS
      * BAH 2023.0908 REMOVED PA-FILE AND GP-PAYEE-PROCESSING CHECKS
      *****************************************************************
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
       77  SCCS-IDENTIFIER PIC X(43) VALUE
           "@(#)./LP/CHCKI0.CBL S35 02/27/24-11:51:51 >".
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
       COPY "LIBLP/LP01BW.CPY".
       COPY "LIBLP/LP01BW_SQL.CPY".
       COPY "LIBLP/LPWSBW.CPY".
       COPY "LIBLP/LP01CA.CPY".
       COPY "LIBLP/LP01CA_SQL.CPY".
       COPY "LIBLP/LPWSCA.CPY".
       COPY "LIBLP/LP01CD.CPY".
       COPY "LIBLP/LP01CD_SQL.CPY".
       COPY "LIBLP/LPWSCD.CPY".
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

       01  TEST-NEXT-CKNOX   PIC X(6).
       01  TEST-CX-ADR       PIC X(25).
       01  TEST-CX-ADRX REDEFINES TEST-CX-ADR.
           03  FILLER        PIC X(24).
           03  TEST-CX-ADR-LAST-BYTE PIC X.

       01  TEST-BS-REFNO.
           03  TEST-BS-REFNOX    OCCURS 5 PIC X(01).
       01  REJECT-DT-RECORD  PIC X.

       01  FORMAT-DS-MSG.
           03  FORMAT-ALP1   PIC X(3).
           03  FORMAT-NO1    PIC 9.
           03  FORMAT-AMT1   PIC ZZZZZZ.99-.
           03  FORMAT-ALP2   PIC X(3).
           03  FORMAT-NO2    PIC 9.
           03  FORMAT-AMT2   PIC ZZZZZZ.99-.
           03  FORMAT-ALP3   PIC X(3).
           03  FORMAT-NO3    PIC 9.
           03  FORMAT-AMT3   PIC ZZZZZZ.99-.
      *-----------------------------------------------------------

       COPY "LIBLP/LPIPBUF.CPY".
       COPY "LIBGB/CAPASSWDW.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBGB/PASSWDW.CPY".
       01  DLRVNO3-SEQ            PIC 9  VALUE 0.
       01  DISPLAY-SEQ            PIC 9  VALUE 1.
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBGB/GPENVW.CPY".
       COPY "LIBGL/ALLENTW.CPY".

       COPY "LIBLP/LPDSCLW.CPY".
       COPY "LIBLP/LPMENUW.CPY".
       COPY "LIBLP/LPSETTYW.CPY".
       COPY "LIBLP/POSTDATEW.CPY".
       COPY "LIBWI/CXSCANW.CPY".
       COPY "LIBWI/DTSCANW.CPY".
       COPY "LIBWI/DLSCANW.CPY".
       COPY "LIBWI/LCSCANW.CPY".
       COPY "LIBWI/BWSCANW.CPY".
       COPY "LIBWI/CDSCANW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBLP/CHCKI_DEF.CPY".
       COPY "LIBLP/LPSMNU_DEF.CPY".
       COPY "LIBLP/LPLNMU_DEF.CPY".
       COPY "LIBLP/LPCAMU_DEF.CPY".
       COPY "LIBLP/LPVDMU_DEF.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBLP/CHCKI_WKS.CPY".
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
       COPY "LIBLP/CHCKI_SCN.CPY".
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

      ************************************
       MAIN-PROGRAM SECTION.
      ************************************
           PERFORM SQL-CONNECT.

      D    STOP MESS.

           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "CHCKI" TO VDU-FORM-NAME.

           PERFORM FORM-RESET.

      * SETUP-INTERRUPT WILL CLEAR THIS FLAG WHICH PREVENTS FURTHER
      * PRESSING OF F10 IF ALREADY IN F10
           MOVE "2" TO VDU-WIMENU-SELECTION.

           PERFORM GET-GPENV.
           PERFORM GET-GLOBAL.

           PERFORM GET-BRANCH.

           MOVE 0 TO Q-POS C-POS.
           MOVE CHCKI-PRU-POS    TO P-POS.
           MOVE "N" TO Q-BUF.
           MOVE "01" TO C-BUF.

           MOVE "S  A020PRU." TO VDU.
           MOVE 00 TO VDUBUF P-BUF IP-PRU.
           PERFORM SCREENIO.

      * INITIALIZED IN CASE OF EXIT BEFORE FD-FILE
      * KEY IS SET
           MOVE 0 TO FD-FMNO.
           MOVE 0 TO IP-FMNO.

       SETRP.
           MOVE "F6-RESET, F7-MENU, ENTER PRINTER NUMBER:" TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "N" TO STAT.
           PERFORM PRU-IN.
           IF STAT = "1<"
              PERFORM PRU-OUT
              GO TO SETRP.
           IF STAT = "1>"
              PERFORM PRU-OUT
              IF INTERRUPT-FG = "Y"
                 MOVE "END" TO IO-TYPE
                 MOVE "E" TO ERRCD
                 MOVE 0 TO LOAN-KEY
                 GO TO END-PROG
              ELSE
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
           IF IP-FMNO NOT NUMERIC OR IP-FMNO = 0
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
           MOVE "E  A020IP." TO VDU.
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
              IF INTERRUPT-FG = "Y"
                 MOVE "END" TO IO-TYPE
                 MOVE "E" TO ERRCD
                 MOVE 0 TO LOAN-KEY
                 GO TO END-PROG
              ELSE
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

           MOVE "S  A250FMX." TO VDU.
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

      * ADDED CXFILE-SCAN-LIST TO IP-BUF AND WILL BE PASSED
      *       CALL XFER USING STAT CXFILE-SCAN-LIST CXFILE-SCAN-FLAG
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
      * ENTER TRANS TYPE;*S=SYSTEM,  *M=MANUAL
      *                   V=VOID,     R=RSV REC
      *                   C=CASH REC, B=BANK           * = VALID
      *---------------------------------------------------------
       ENTER-TRANS-TYPE.
           MOVE "N" TO CXFILE-SCAN-FLAG.
           MOVE 0 TO NEW-FLAG CHECK-NO.
           MOVE SPACES TO IO-TYPE.

           PERFORM NEW-RECORD-SETUP.

      * F7-MENU, ^-BACK, M-MANUAL CHECK, S-SYSTEM CHECK:
           MOVE MESS-1 TO MESS.
           PERFORM SEND-LEGEND.

           PERFORM DISPLAY-SCREEN-HEADER.
           MOVE "E  A010K010." TO VDU.
           PERFORM SCREENIO.
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

      *  TYPE-SUB 1 = MANUAL CHECK
      *  TYPE-SUB 2 = SYSTEM CHECK
           MOVE 2 TO TYPE-MAX.
           PERFORM VERIFY-TRANS-TYPE.
           IF TYPE-SUB > TYPE-MAX
              GO TO ENTER-TRANS-TYPE.

           PERFORM DISPLAY-SCREEN-HEADER.

      *---------------------------------------------------------
       ENTER-PAYEE-TYPE.
      *---------------------------------------------------------
           MOVE " " TO CXFILE-SCAN-FLAG.
           IF TRANS-TYPE = "M"
              IF BR-NO-OUT-DLR-RSV = "N"
                 MOVE MESS-2B1 TO MESS
              ELSE
                 MOVE MESS-2B TO MESS
              END-IF
           ELSE
              IF BR-NO-OUT-DLR-RSV = "N"
                 MOVE MESS-2A1 TO MESS
              ELSE
                 MOVE MESS-2A TO MESS.
           PERFORM SEND-LEGEND.

           MOVE "E  A020K020." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO EXIT-ENTER-KEYS.
           IF TRANS-TYPE NOT = "M"
              IF VDUSTATUS = "16"
                 PERFORM ALIGNMENT-PRINT
                 GO TO ENTER-PAYEE-TYPE.
           IF VDUSTATUS = "1>"
              GO TO MENU-KEY.
           IF VDUSTATUS = "1U"
              GO TO ENTER-TRANS-TYPE.
           IF VDUSTATUS = "1:"
              PERFORM NON-WIN-CX-SCAN
              IF CXFILE-SCAN-FLAG = "Y"
                 MOVE " " TO IO-TYPE
                 MOVE CX-TYPE TO HOLD-CHECK-PAYEE
                 GO TO ENTER-CHECK-NO
              ELSE
                 MOVE "CLR" TO IO-TYPE
                 GO TO EXIT-ENTER-KEYS.

      *---------------------------------------------------------
      * SCAN CX-FILE, TEMPORARY CHECK RECORDS
      * GLB DONT LIKE THIS WAY TO SAVE
      *---------------------------------------------------------
           IF VDUSTATUS = "1<"
              PERFORM CXFILE-SCAN
              IF CXFILE-SCAN-FLAG = "Y"
                 MOVE " " TO IO-TYPE
                 MOVE CX-TYPE TO HOLD-CHECK-PAYEE
                 GO TO ENTER-CHECK-NO
              ELSE
                 MOVE HOLD-TRANS-TYPE  TO CX-PAID
                 MOVE HOLD-CHECK-PAYEE TO VDUBUF.

           IF VDUSTATUS NOT = "00"
      *       PERFORM DISPLAY-SCREEN-HEADER
              GO TO ENTER-PAYEE-TYPE.
      *    IF VDUBUF = SPACES
      *       MOVE HOLD-CHECK-PAYEE TO VDUBUF.

           MOVE VDUBUF TO CX-TYPE CHECK-PAYEE HOLD-CHECK-PAYEE.

      * CS DONT ALLOW RR,RC,RP OR RD WHICH ARE RECEIPT TRANSACTIONS
            IF DEALER-RSV-REC OR DEALER-PROC-REC
              GO TO ENTER-PAYEE-TYPE.

           IF VDUBUF = "VX" OR "CA" OR "BK"
              GO TO ENTER-PAYEE-TYPE.

           IF BR-NO-OUT-DLR-RSV = "N"
              IF (DEALER-RSV)
                 MOVE "WITHDRAWL FROM RESERVE NOT ALLOWED" TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-PAYEE-TYPE.

      * SWITCH CX-TYPE FOR INTERNAL USE FROM "DL" TO "VX":
           IF CX-TYPE = "DL"
              IF CXFILE-SCAN-FLAG NOT = "Y"
                 MOVE "VX" TO CX-TYPE CHECK-PAYEE.

           PERFORM VERIFY-CHECK-PAYEE.
           IF PAYEE-SUB > PAYEE-MAX
              GO TO ENTER-PAYEE-TYPE.

      *    PERFORM DISPLAY-SCREEN-HEADER.

           IF NOT (CX-TYPE = "DR" OR "DC")
              MOVE "S  A010K021." TO VDU
              MOVE SPACES TO VDUBUF
              PERFORM SCREENIO
              GO TO ENTER-CHECK-NO.

           MOVE "S  A060P111." TO VDU.
           MOVE SPACES      TO VDUBUF.
           PERFORM SCREENIO.
           MOVE "S  A020111." TO VDU.
           MOVE SPACES        TO VDUBUF.
           PERFORM SCREENIO.

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

           MOVE VDUNUM0 TO CX-DLRVNO
                           DLRVNO HOLD-DLRVNO.
           IF CX-DLRVNO < 1 OR > 3
              GO TO ENTER-DLRVNO.

           IF CX-DLRVNO = 3
              MOVE DLRVNO3-SEQ TO PASSWDW-SEQ
              MOVE "TO ACCESS RESERVE 3" TO PASSWDW-PROMPT2
              PERFORM VERIFY-PASSWORD
              IF PASSWDW-STAT NOT = "OK"
                 GO TO ENTER-DLRVNO.

      *---------------------------------------------------------
       ENTER-CHECK-NO.
      *---------------------------------------------------------
           MOVE "F7-MENU, F1-CLR, ^-BACK, ENTER CHECK NO:"
                 TO MESS.
           PERFORM SEND-LEGEND.

           IF TRANS-TYPE = "S"
              IF BR-AUTO-CHECK = "Y"
      * MUST READ BRANCH AGAIN, BECAUSE WHEN YOU COME BACK FROM
      * PRINTING THE FIRST ONE, YOU NEED THE UPDATED NUMBER
                 PERFORM GET-BRANCH
                 MOVE "SN N060K030." TO VDU
                 MOVE BR-NEXT-CHECK TO VDUNUM0
                 PERFORM SCREENIO
                 GO TO TEST-CHECK-NO.

           MOVE "EN N060K030." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO EXIT-ENTER-KEYS.
           IF VDUSTATUS = "1>"
              GO TO MENU-KEY.
           IF VDUSTATUS = "1U"
              IF CXFILE-SCAN-FLAG = "Y"
                 MOVE "CLR" TO IO-TYPE
                 GO TO EXIT-ENTER-KEYS
              ELSE
                 GO TO ENTER-PAYEE-TYPE.
           IF VDUSTATUS NOT = "00"
              GO TO ENTER-CHECK-NO.

           INSPECT VDUNUM0 REPLACING ALL " " BY "0".
           IF VDUNUM0 = 0
              GO TO ENTER-CHECK-NO.

       TEST-CHECK-NO.
           MOVE VDUNUM0 TO CK-NO   CX-CKNO CHECK-NO.
           MOVE BRANCH  TO CK-BRNO CX-BRNO.
           PERFORM OPEN-CK1-FILE.
           PERFORM READ-CK1-FILE.
           PERFORM CLOSE-CK1-FILE.
           IF IO-FG = 0
              MOVE DISPLAY-BUF TO HOLD-DISPLAY-BUF
              PERFORM DISPLAY-CK-FIELDS
              MOVE "CHECK ALREADY EXISTS" TO MESS
              PERFORM SEND-MESS
              MOVE HOLD-DISPLAY-BUF TO DISPLAY-BUF
              MOVE HOLD-TRANS-TYPE  TO CX-PAID
              MOVE HOLD-CHECK-PAYEE TO CX-TYPE
              MOVE HOLD-DLRVNO      TO CX-DLRVNO
              IF HOLD-TRANS-TYPE = "S"
                 IF BR-AUTO-CHECK = "Y"
                    MOVE "PLEASE CORRECT NEXT CHECK NUMBER" TO MESS
                    PERFORM SEND-MESS
                    PERFORM REFRESH-SCREEN
                    GO TO ENTER-TRANS-TYPE
                 ELSE
                    PERFORM REFRESH-SCREEN
                    GO TO ENTER-CHECK-NO
              ELSE
                 PERFORM REFRESH-SCREEN
                 GO TO ENTER-CHECK-NO.

      * MANUAL CHECKS DURING AUTO CHECK NUMBERING MUST HAVE A NUMBER
      * LESS THAN THE NEXT BRANCH CHECK NUMBER. THEY MUST RIP THE CHECK
      * OFF PRINTER, ADVANCE THE NEXT-CHECK NUMBER IN BR-REC, THEN THEY
      * INPUT THE CORRECT NUMBER, BECAUSE IT WILL BE 1 NUMBER LESS.
           IF HOLD-TRANS-TYPE = "M"
              IF BR-AUTO-CHECK = "Y"
                 IF CX-CKNO NOT < BR-NEXT-CHECK
                    MOVE "INVALID MANUAL CHECK NUMBER" TO MESS
                    PERFORM SEND-MESS
                    GO TO ENTER-CHECK-NO.

      *---------------------------------------------------------
      *  CX-RECORD FOUND FROM SCAN
      *---------------------------------------------------------
           IF CXFILE-SCAN-FLAG = "Y"
              MOVE 2 TO NEW-FLAG
              GO TO EXIT-ENTER-KEYS.

      *---------------------------------------------------------
      *  ENTER NEW CX-RECORD INFO
      *---------------------------------------------------------
           MOVE "S  A000NEW." TO VDU.
           MOVE "* * NEW * *" TO VDUBUF.
           PERFORM SCREENIO.

      * GLB  03/20/03
      *---------------------------------------------------------
       ENTER-TILLNO.
      *---------------------------------------------------------
           IF NOT (DRAWCK)
              GO TO EXIT-ENTER-KEYS.

           IF BR-PASSWORD-DRAWER = "Y"
              MOVE "F7-MENU, ^-BACK, ENTER CASH DRAWER PASSWORD:"
                              TO MESS
           ELSE
              MOVE "F7-MENU, ^-BACK, ENTER CASH DRAWER NO:"
                              TO MESS.
           PERFORM SEND-LEGEND.

      * - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       ACCEPT-ENTER-TILLNO.
      * GLB 07/21/99
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
              MOVE "ENNN020TILL." TO VDU
              PERFORM SCREENIO.
       ACCEPT-CHECK-VDUSTATUS.
      * - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF VDUSTATUS = "1>"
              GO TO MENU-KEY.
           IF VDUSTATUS = "1U"
              IF TRANS-TYPE = "C"
                 GO TO ENTER-TRANS-TYPE
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

      *---------------------------------------------------------
      *   E X I T      P R O G R A M
      *---------------------------------------------------------
       MENU-KEY.
           PERFORM PRU-OUT.
           IF INTERRUPT-FG = "Y"
              MOVE "END" TO IO-TYPE
              MOVE "E" TO ERRCD
              MOVE 0 TO LOAN-KEY
              GO TO END-PROG.

           DESTROY ALL CONTROLS
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
           MOVE "Y" TO CHFG(12) CHFG(13) CHFG(14)
                        CHFG(15) CHFG(16).
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
           IF (DEALER-RSV)
              PERFORM TEST-DLR-RESERVES
              IF RSV-NEGATIVE NOT = " "
                 GO TO MODIFY-ROUTINE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * TEST FOR LOAN IN RESCISSION AND IF 'CHECK' PROGRAM SELECTED
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF CX-IN-RESCISSION
              MOVE MESSAGE-RESCISSION TO MESS
              PERFORM SEND-MESS
              GO TO MODIFY-ROUTINE.

           PERFORM CHECK-GL-BALANCE.
           IF BAL-FG = 9
              IF (DEALER-RSV)
                 MOVE "DEALER G/L DISTRIBUTION MUST NET TO ZERO"
                                                         TO MESS
                 PERFORM SEND-MESS
              ELSE
                 MOVE "CASH NOT = G/L DISTRIBUTION" TO MESS
                 PERFORM SEND-MESS
              END-IF
              GO TO MODIFY-ROUTINE.

      * MANUAL CHECK, TEST CHECK NUMBER AGAIN, DUPLICATES HAVE BEEN
      * SLIPPING THRU.
           IF TYPE-SUB = 1
              MOVE CX-BRNO TO CK-BRNO
              MOVE CX-CKNO TO CK-NO
              PERFORM OPEN-CK1-FILE
              PERFORM READ-CK1-FILE
              PERFORM CLOSE-CK1-FILE
              IF IO-FG = 0
                 MOVE "DUPLICATE CHECK NUMBER !" TO MESS
                 PERFORM SEND-MESS
                 GO TO MODIFY-ROUTINE.

      *SYSTEM CHECK, AUTO CHECK NUMBER TURNED ON, CHECK AGAIN TO MAKE
      *SURE THE CX-CHECKNO MATCHES THE NEXT CHECK NUMBER IN THE BRANCH
           IF TYPE-SUB = 2 AND BR-AUTO-CHECK = "Y"
              PERFORM OPEN-BR-FILE
              PERFORM READ-BR-FILE
              PERFORM CLOSE-BR-FILE
              IF IO-FG NOT = 0
                 GO TO IO-ERROR
              ELSE
                 IF CX-CKNO NOT = BR-NEXT-CHECK
                    MOVE BR-NEXT-CHECK TO TEST-NEXT-CKNOX
                    STRING 
                  "CHECK NUMBER HAS CHANGED, NEW NUMBER IS "
                                        TEST-NEXT-CKNOX
                   DELIMITED BY "  " INTO MESS
                   PERFORM SEND-MESS
                   GO TO MODIFY-ROUTINE.

      *---------------------------------------------------------
      *    U P D A T E  /  P R I N T    C H E C K S
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
           IF ELE = 1
              MOVE 0 TO HOLD-DT-DLNO
              IF (DEALER)
                 MOVE "F1, F6-SCAN, ENTER DEALER NO:" TO MESS
                 PERFORM SEND-LEGEND
              END-IF
              IF (LOANCK)
                 MOVE "F1, F6-SCAN, ENTER BORROWER SSNO:" TO MESS
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
              MOVE "F1, ENTER REASON FOR CHECK:" TO MESS
              PERFORM SEND-LEGEND.
           IF ELE = 10
              MOVE "F1, ENTER AMOUNT OF THE CHECK:" TO MESS
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

           IF ELE = 2
              PERFORM CLASS-SCAN
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
              IF (LOANCK)
                 PERFORM BORROWER-SCAN
                 IF IO-FG = 9
                    GO TO ENTER-ELE.

           IF ELE NOT = 2
              MOVE 1 TO ONE
              GO TO GOTO-FIELD.

      *----------------------------------------------
      * SCAN DETAIL DT-FILE FOR SF DEALER LOAN#
      *----------------------------------------------
           MOVE 3           TO DTSCANW-TYPE.
           MOVE BRANCH      TO DTSCAN-BRNO.
           MOVE CX-DLNO     TO DTSCANW-DLNO.
           MOVE CX-CLASS    TO DTSCAN-CLASS.
           MOVE CX-ACCTNO   TO DTSCAN-ACCTNO.
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

           PERFORM SETUP-CX-FROM-DT.
      *    PERFORM DISPLAY-DEALER-STATS.
           PERFORM DISPLAY-SCREEN-BODY.

           MOVE 1 TO ONE.
           GO TO GOTO-FIELD.

       TEST-F7.
           IF VDUSTATUS NOT = "1>"
              GO TO TEST-00.
           PERFORM PRU-OUT.
           IF INTERRUPT-FG = "Y"
              MOVE "END" TO IO-TYPE
              MOVE "E" TO ERRCD
              MOVE 0 TO LOAN-KEY
              GO TO END-PROG.

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
           IF (DEALER)
              GO TO F-01DL
           ELSE
           IF (LOANCK)
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
              IF (DEALER-RSV)
                 IF LC-SERVACCT = "Y"
                    MOVE "CAN'T BE A SERVICE ACCOUNT CLASS!" TO MESS
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

           IF (LOANCK)
              GO TO CONTINUE-F-021B.

           PERFORM DISPLAY-DEALER-STATS.

      *    DT-FILE IDENTIFIED, DT-SCAN USED, LOAN MASTER FOUND
           IF IO-FG = 0
              IF HOLD-DT-DLNO NOT = 0
                 GO TO CONTINUE-F-021A.

      * GLB 09/27/01
      * IF A LOAN NUMBER WAS ENTERED, NEED TO VALID DT-RECORD
           IF CX-ACCTNO NOT = 0
              PERFORM VALIDATE-DT-RECORD
              IF REJECT-DT-RECORD = "N"
                 GO TO CONTINUE-F-021A
              END-IF
              PERFORM SEND-REJECT-MESS
              GO TO ENTER-ELE.

      * IF A 0 LOAN NUMBER WAS ENTERED, CREATE DT-RECORD
           IF CX-ACCTNO = 0
              MOVE 0 TO HOLD-DT-DLNO
              MOVE "CREATING DEALER LOAN DETAIL" TO MESS
                  PERFORM SEND-MESS
              PERFORM CLEAR-DT-FILE
              MOVE CX-DLNO     TO DT-DLNO
              MOVE CX-CLASS    TO DT-CLASS
              MOVE CX-ACCTNO   TO DT-ACCTNO
              MOVE CX-BRNO     TO DT-BRNO
              MOVE TRANS-DATE  TO DT-DATE
      * GLB 08/16/01
              IF (DEALER-PROCEEDS)
                 MOVE "N" TO DT-UNPAID.

       CONTINUE-F-021A.
           IF DT-PAID = "B"
              IF DT-BATCHID = 0
                 MOVE "WARNING, ORIGINALLY RELEASED TO BATCH"
                 TO MESS.
           PERFORM SETUP-CX-FROM-DT.

       CONTINUE-F-021B.
           PERFORM DISPLAY-SCREEN-BODY.
           PERFORM SEND-GLBAL.

           GO TO CREATE-ROUTINE.

       F-03.
           MOVE VDUBUF TO CX-PAYEE.
      *    MUST ENTER PAYEE
           IF VDUBUF = " "
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       F-04.
           MOVE VDUBUF TO CX-ADR1 TEST-CX-ADR.
           IF TEST-CX-ADR-LAST-BYTE NOT = SPACES
              MOVE "WARNING: LAST POSITION WILL BE LOST, USE ADR2"
               TO MESS
              PERFORM SEND-MESS.
           GO TO CREATE-ROUTINE.
       F-05.
           MOVE VDUBUF TO CX-ADR2 TEST-CX-ADR.
           IF TEST-CX-ADR-LAST-BYTE NOT = SPACES
              MOVE "WARNING: LAST POSITION WILL BE LOST" TO MESS
              PERFORM SEND-MESS.
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
           IF VDUNUM2 NOT > 0
               GO TO ENTER-ELE.
           IF (DEALER-RSV)
              PERFORM TEST-DLR-RESERVES
              IF RSV-NEGATIVE NOT = " "
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
      *  PER LORI VALIDATE--------
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

      **************************************
      *    DISPLAY SCANNED CX-RECORD
      **************************************
       DISPLAY-CX-REC SECTION.

           MOVE DISPLAY-SEQ TO PASSWDW-SEQ.
           MOVE "TO DISPLAY EXISTING CHECK" TO PASSWDW-PROMPT2.
           PERFORM VERIFY-PASSWORD.
           IF PASSWDW-STAT NOT = "OK"
              GO TO EXIT-DISPLAY-CK-FIELDS.

           MOVE "S  A000K020." TO VDU.
           MOVE CX-TYPE TO CHECK-PAYEE VDUBUF.
           PERFORM SCREENIO.
           PERFORM VERIFY-CHECK-PAYEE.

           MOVE HOLD-TRANS-TYPE  TO CX-PAID.
           PERFORM VERIFY-TRANS-TYPE.

           MOVE CX-DLRVNO TO DLRVNO.

           PERFORM DISPLAY-SCREEN-BODY.

           PERFORM SEND-GLBAL.

      * DISPLAY TO TOP OF SCREEN
           MOVE SPACES   TO LPWSFM-EDIT-FM-INITKEY.
           MOVE CX-BRNO  TO LPWSFM-EDIT-FM-BRNO.
           MOVE CX-TYPE  TO LPWSFM-EDIT-FM-TYPE.
           MOVE CX-SEQNO TO LPWSFM-EDIT-FM-SEQNO.
           MOVE 0        TO LPWSFM-EDIT-FM-CKFMNO.

           MOVE LPWSFM-EDIT-FM-INITKEY TO WR-BUF.
           MOVE EDIT-FM-LIST           TO WR-LIST.
           PERFORM CALL-WRFORM.

       EXIT-DISPLAY-CK-FIELDS.
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
           MOVE SPACES TO CDSCAN-WINDOW-BUF.
           MOVE "BS"   TO CDSCAN-TYPE.

           MOVE "WI/CDSCAN" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH CDSCAN-BUF EXIT-PATHNAME.
           CANCEL FORM-PROGX.
           MOVE CDSCAN-CODE TO TEST-BS-REFNO.
           IF ( CDSCAN-STATUS      NOT = "00"    ) OR
              ( CDSCAN-CODE = SPACES  ) OR
              (( TEST-BS-REFNOX(1) = SPACES )  AND
               ( TEST-BS-REFNOX(2) = SPACES )  AND
               ( TEST-BS-REFNOX(3) = SPACES )  AND
               ( TEST-BS-REFNOX(4) = SPACES ))
              MOVE 9 TO IO-FG
              GO TO REFNO-SCAN-EXIT.

           MOVE 0 TO IO-FG.

           MOVE "S  A050111." TO VDU.
           MOVE CDSCAN-CODE   TO VDUBUF.
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

      ****************************************
      *    CXFILE WINDOW SCAN
      ****************************************
       CXFILE-SCAN SECTION.
           MOVE BRANCH TO CXSCANW-BRNO.

           MOVE "WI/CXSCAN " TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH CXSCAN-BUF EXIT-PATHNAME.
           CANCEL FORM-PROGX.

           IF CXSCAN-STATUS NOT = "00"
              MOVE 9 TO IO-FG
              GO TO EXIT-CXFILE-SCAN.

           MOVE CXSCAN-CX1-KEY TO CX1-KEY.
           PERFORM OPEN-CX1-FILE.
           PERFORM READ-CX1-FILE.
           PERFORM CLOSE-CX1-FILE.
           IF IO-FG = 9
              GO TO EXIT-CXFILE-SCAN.

           PERFORM DISPLAY-CX-REC.
           MOVE LC-SERVACCT TO CX-SERVACCT.

           MOVE "Y" TO CXFILE-SCAN-FLAG.
           PERFORM SET-SCAN-CHFG-TABLE.

           MOVE 0 TO IO-FG.

       EXIT-CXFILE-SCAN.
           EXIT.

      ********************************************************
      *    OLD NON WINDOW SCAN FUNCION OF UNPRINTED CHECKS
      ********************************************************
       NON-WIN-CX-SCAN SECTION.
           MOVE "Y" TO FIRST-SCAN.
           MOVE "S  A090PFMKEY:S." TO VDU.
           MOVE "SCAN KEY:" TO VDUBUF.
           PERFORM SCREENIO.
           PERFORM OPEN-CX1-FILE.

           MOVE CX-PATH-OWNBR TO CX-BRNO.
           MOVE SPACES        TO CX-TYPE.
           MOVE 0             TO CX-SEQNO.

           PERFORM START-CX1-FILE.

           IF TRANS-TYPE NOT = "M" AND
                          NOT = "S"
              GO TO EXIT-NON-WIN-CX-SCAN.

       NON-WIN-CX-SCAN-NEXT.
           PERFORM READ-CX1-FILE-NEXT.
      *    UNLOCK CX-FILE.
           IF IO-FG = 9 OR (CX-BRNO NOT = CX-PATH-OWNBR) 
              IF FIRST-SCAN = "Y"
                 MOVE "NO RECORDS" TO MESS
                 PERFORM SEND-MESS
                 MOVE "CLR" TO IO-TYPE
                 GO TO EXIT-NON-WIN-CX-SCAN
              ELSE
                 GO TO NON-WIN-CX-SCAN.

           MOVE "N" TO FIRST-SCAN.
           PERFORM DISPLAY-CX-REC.

       NON-WIN-CX-SCAN-CONT.
           MOVE "F1-CANCEL, F5-SCAN, RTN-ACCEPT:" TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "R RA060SEL." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO EXIT-NON-WIN-CX-SCAN.
           IF VDUSTATUS = "1:"
              GO TO NON-WIN-CX-SCAN-NEXT.
           IF VDUSTATUS NOT = "00"
              GO TO NON-WIN-CX-SCAN-CONT.
           IF VDUBUF NOT = SPACES
              GO TO NON-WIN-CX-SCAN-CONT.
           MOVE "Y" TO CXFILE-SCAN-FLAG.
           PERFORM SET-SCAN-CHFG-TABLE.

       EXIT-NON-WIN-CX-SCAN.
           PERFORM CLOSE-CX1-FILE.

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

           IF (LN-POCD-VOID) 
              MOVE "ACCOUNT HAS BEEN DELETED" TO MESS
              PERFORM SEND-MESS
              MOVE 9 TO IO-FG
              GO TO EXIT-VERIFY-LOAN-CLASS.

           IF (DEALER)
              IF LN-SALEFIN = "N"
                 MOVE "LOAN IS NOT SALES FINANCE" TO MESS
                 PERFORM SEND-MESS
                 MOVE 9 TO IO-FG
                 GO TO EXIT-VERIFY-LOAN-CLASS.

           IF CX-CLASS NOT = LN-CLASS
      * DEALER RESERVE CHECKS MUST HAVE SALES FINANCE CLASS ONLY, CAN NOT
      * AUTOMATICALLY SWITCH THEM TO THE LOAN CLASS WITHOUT VERIFYING! #944
              IF NOT (DEALER-RSV)
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

           IF (LOANCK)
              IF CX-SSNO NOT = LN-SSNO(1)
                 MOVE LN-SSNO(1) TO CX-SSNO NUM-BW
             MOVE "BORROWER MISMATCH!, SWITCHING TO SS# &&&&&&&&&"
                 TO MESS
                 INSPECT MESS REPLACING FIRST "&&&&&&&&&" BY NUM-BW
                 PERFORM SEND-MESS
                 MOVE 8 TO IO-FG.

       EXIT-VERIFY-LOAN-CLASS.
           EXIT.

      *********************************
      *   V E R I F Y   T Y P E       *
      *   VERIFY TYPE CODE;           *
      *   1 =  MANUAL CHECK           *
      *   2 =  SYSTEM CHECK           *
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
           MOVE P-CODE(PAYEE-SUB) TO PAYEE-CODE.
           MOVE P-DESC(PAYEE-SUB) TO VDUBUF PAYEE-DESC.

           MOVE CHFG-PTR(TYPE-SUB, PAYEE-SUB) TO SUB.
           MOVE CHFG-VAL(SUB) TO CHFG-TABLE.


           IF (BANKTR)
              IF CX-DEPOSIT-WITHDR = "D"
                 MOVE "BANK DEPOSIT" TO PAYEE-DESC VDUBUF.

           PERFORM SCREENIO.
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

      * GLB  03/20/03 FORCE G/L FOR DRAWER DEPOSIT CHECK
           IF (DRAWCK)
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
           PERFORM OPEN-CX1-FILE.
           PERFORM UPDATE-FILES-CHECKTRANS.
           PERFORM CLOSE-CX1-FILE.
           IF CXFILE-SCAN-FLAG = "Y"
              MOVE CX-REC TO SAVE-CX-REC
              PERFORM OPEN-CX1-FILE
              PERFORM READ-CX1-FILE
              IF IO-FG = 0
                 PERFORM DELETE-CX1-FILE
              END-IF
              PERFORM CLOSE-CX1-FILE
              PERFORM REMOVE-FM1-REQUEST.
           MOVE "CLR" TO IO-TYPE.
           GO TO EXIT-UPDATE-ROUTINE.

       UPDATE-SYSTEM.
      *---------------
           PERFORM REQUEST-CX1-FILE.
           PERFORM UPDATE-FM1-FILE.
           PERFORM SET-LOANKEY.
           MOVE "END" TO IO-TYPE.
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
           MOVE "CHCKI0" TO PASSWDW-PGM.
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


      ******************************************
       REMOVE-FM1-REQUEST SECTION.
      ******************************************

           MOVE "REMOVING FORM PRINT REQUEST........." TO MESS.
           PERFORM SEND-LEGEND.

           PERFORM OPEN-FM1-FILE.

           MOVE SPACES        TO LPWSFM-EDIT-FM-INITKEY.
           MOVE SAVE-CX-BRNO  TO LPWSFM-EDIT-FM-BRNO.
           MOVE SAVE-CX-TYPE  TO LPWSFM-EDIT-FM-TYPE
           MOVE SAVE-CX-SEQNO TO LPWSFM-EDIT-FM-SEQNO.
           MOVE 0             TO LPWSFM-EDIT-FM-CKFMNO.

           MOVE FM-PATH-OWNBR          TO FM-BRNO.
           MOVE LPWSFM-EDIT-FM-INITKEY TO FM-INITKEY.
           MOVE 0                      TO FM-FMNO.

           PERFORM START-FM1-FILE.

       NEXT-CHECK-REQUEST.
           PERFORM READ-FM1-FILE-NEXT.
           MOVE FM-INITKEY TO LPWSFM-EDIT-FM-INITKEY.
           IF IO-FG = 0
              IF (LPWSFM-EDIT-FM-BRNO NOT = SAVE-CX-BRNO) OR
                  (LPWSFM-EDIT-FM-TYPE NOT = SAVE-CX-TYPE)
                    MOVE 9 TO IO-FG.

           IF IO-FG = 9 OR (FM-BRNO NOT = FM-PATH-OWNBR)
              MOVE "UNABLE TO FIND CHECK FORM REQUEST (FMFILE)" TO MESS
              PERFORM SEND-LEGEND
              GO TO EXIT-REMOVE-FM1.

           IF (LPWSFM-EDIT-FM-SEQNO NOT = CX-SEQNO)
      *       UNLOCK FM-FILE
              GO TO NEXT-CHECK-REQUEST.

           MOVE "Y" TO FM-PRINT.

           PERFORM REWRITE-FM1-FILE.
       EXIT-REMOVE-FM1.
           EXIT.

      ********************************************
       UPDATE-FM1-FILE SECTION.
      ********************************************

           IF CXFILE-SCAN-FLAG = "Y"
              GO TO EXIT-UPDATE-FM1.

           MOVE "UPDATING FORM REQUEST FILE............" TO MESS.
           PERFORM SEND-LEGEND.

           PERFORM OPEN-FM1-FILE.
          
           MOVE FM-PATH-OWNBR TO FM-BRNO.

           MOVE SPACES        TO LPWSFM-EDIT-FM-INITKEY.
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

      *****************************
      *     DISPLAY FIELDS        *
      *****************************
       DISPLAY-CK-FIELDS SECTION.

           MOVE CK-PAID TO CX-PAID.
           MOVE CK-TYPE TO CX-TYPE.
           MOVE CK-DLRVNO TO CX-DLRVNO.
           PERFORM DISPLAY-SCREEN-HEADER.

           MOVE " " TO DISPLAY-BUF.

           IF (DEALER)
               MOVE CK-DLNO TO D-0101.

           MOVE CK-CLASS    TO D-020.
           MOVE CK-ACCTNO   TO L-ACCTNO.
           MOVE L-NUMBER    TO D-021.
           MOVE CK-PAYEE    TO D-030.
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


      *---------------------------------------------------------
       DISPLAY-DEALER-MASTER SECTION.
      *---------------------------------------------------------
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
           MOVE DL-NAME TO D-030.
           MOVE DL-ADR1 TO D-040 TEST-CX-ADR.
           IF TEST-CX-ADR-LAST-BYTE NOT = SPACES
              MOVE "WARNING: LAST POSITION OF DL ADR1 IS LOST"
                   TO MESS
              PERFORM SEND-MESS.
           MOVE DL-ADR2 TO D-050 TEST-CX-ADR.
           IF TEST-CX-ADR-LAST-BYTE NOT = SPACES
              MOVE "WARNING: LAST POSITION OF DL ADR2 IS LOST"
                   TO MESS
              PERFORM SEND-MESS.
           MOVE DL-CITY TO D-060.
           MOVE DL-STATE TO D-070.
           MOVE DL-ZIP TO D-080.
           PERFORM DISPLAY-NAME-ADDR.

           IF (DL-TERMDATE NOT > TRANS-DATE) AND DL-TERMDATE NOT = 0
              MOVE " " TO MESS
              MOVE DL-TERMDATE TO DATE-YYYYMMDD
              PERFORM CONVERT-YYYYMMDD-TO-ALPDATE
              MOVE DL-TERMDATE TO VDU-DATE-EDIT
              STRING "DEALER TERMINATED ON " ALP-DATE
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
       DISPLAY-SCREEN-BODY SECTION.
      *---------------------------------------------------------
           IF (DEALER)
              MOVE CX-DLNO      TO D-0101.
           MOVE CX-CLASS        TO D-020.
           MOVE CX-CLASS        TO LC-CODE.
           PERFORM GET-LC.
           MOVE LC-DESC         TO D-CLDESC.
           MOVE CX-ACCTNO       TO D-021.

           MOVE CX-PAYEE TO D-030.
           MOVE CX-ADR1 TO D-040.
           MOVE CX-ADR2 TO D-050.
           MOVE CX-CITY TO D-060.
           MOVE CX-STATE TO D-070.
           MOVE CX-ZIP TO D-080.

      * BAH REMOVED 9/25/01
      *    IF (DEALER)
      *       IF LN-SSNO(1) NOT = 0
      *          MOVE CX-ACCTNO TO LN-ACCTNO
      *          PERFORM SETUP-DEALER-BORROWER
      *       ELSE
      *          MOVE SPACES TO INSPECT-W
      *       END-IF
      *       MOVE INSPECT-W       TO D-BWNAME
      *       PERFORM DISPLAY-DEALER-MASTER.

      *    IF (LOANCK)
      *       MOVE CX-SSNO TO BW-SSNO
      *       PERFORM DISPLAY-BORROWER.

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
              MOVE CX-BATCHID TO D-09B-BATCHID.

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
       VALIDATE-DT-RECORD SECTION.
           MOVE "Y" TO REJECT-DT-RECORD.

           PERFORM OPEN-DT1-FILE.

           IF HOLD-DT-DLNO NOT = 0
              MOVE HOLD-DT1-KEY TO DT1-KEY
           ELSE
              PERFORM CLEAR-DT-FILE
              MOVE CX-BRNO       TO DT-BRNO
              MOVE CX-DLNO       TO DT-DLNO
              MOVE CX-CLASS      TO DT-CLASS
              MOVE CX-ACCTNO     TO DT-ACCTNO
              MOVE 19000101      TO DT-DATE
              MOVE 0             TO DT-SEQ.

           MOVE DT-BRNO    TO DT-BRNO
                              QDT1-WBEG-BRNO
                              QDT1-WEND-BRNO.
           MOVE DT-NUM     TO QDT1-WBEG-NUM
                              QDT1-WEND-NUM.
           MOVE DT-CLASS   TO QDT1-WBEG-CLASS
                              QDT1-WEND-CLASS.
           MOVE DT-ACCTNO  TO QDT1-WBEG-ACCTNO
                              QDT1-WEND-ACCTNO.
           MOVE DT-DATE    TO SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD  TO QDT1-WBEG-DATE.
           MOVE EXT-JULIAN-END TO SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD  TO QDT1-WEND-DATE.

           MOVE DT-SEQ     TO QDT1-WBEG-SEQ.
           MOVE ALL "9"    TO QDT1-WEND-SEQ.
 
           PERFORM START-DT1-FILE.
       FIND-DT-NEXT.
           PERFORM READ-DT1-FILE-NEXT.
           IF (IO-FG = 9) OR (DT-BRNO NOT = CX-BRNO)
              GO TO EXIT-TRY-FIND-DT.

           IF DT-DLNO NOT = CX-DLNO
              GO TO EXIT-TRY-FIND-DT.
           IF DT-CLASS NOT = CX-CLASS
              GO TO EXIT-TRY-FIND-DT.
           IF DT-ACCTNO NOT = CX-ACCTNO
              GO TO EXIT-TRY-FIND-DT.

           MOVE DT-TRCD TO CHECK-TRCD.
           IF NOT (DEALER-ORIGINAL)
                 GO TO FIND-DT-NEXT.

      *  DELETE CHECKED WHEN LOAN READ
      *    IF DT-REVERSE = "Y"
      *       MOVE "THIS ACCOUNT HAS BEEN   D E L E T E D"
      *       GO MESS
      *       PERFORM SEND-MESS
      *       GO TO EXIT-TRY-FIND-DT.


      * A DT-RECORD ASSIGNED TO BATCH, BUT NOT RELEASED IS VALID
      *    IF DT-STATUS  = "B"
      *       IF DT-BATCHID = 0
      *          GO TO OKAY-DT-RECORD.

      * IF PROCEEDS CHECK, TEST STATUS
      * ANY OTHER STATUS (R, P, ), IS NOT VALID
           IF (DEALER-PROCEEDS)
              IF DT-STATUS NOT = " "
                 IF HOLD-DT-DLNO = 0
                    GO TO FIND-DT-NEXT
                 END-IF
                 GO TO EXIT-TRY-FIND-DT.

       OKAY-DT-RECORD.
           MOVE DT1-KEY TO HOLD-DT1-KEY.
           MOVE "N" TO REJECT-DT-RECORD.

       EXIT-TRY-FIND-DT.
           PERFORM CLOSE-DT1-FILE.

      *---------------------------------------------------------
      *  TESTING DEALER RESERVE AMOUNTS IN DS-FILE
      *          SUFFICIENT TO WRITE CHECK ?
      *---------------------------------------------------------
       TEST-DLR-RESERVES SECTION.

           MOVE " " TO RSV-NEGATIVE.
           PERFORM OPEN-DS1-FILE.

           MOVE TRANS-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-YYYYMM.
           MOVE DATE-YYYYMM TO DS-DATE-CCYYMM.

           MOVE CX-BRNO TO DS-BRANCH.
           MOVE CX-DLNO TO DS-DLNO.
           MOVE CX-CLASS TO DS-CLASS.
           PERFORM READ-DS1-FILE.
           PERFORM CLOSE-DS1-FILE.
           IF IO-FG NOT = 0
              MOVE "Y" TO RSV-NEGATIVE
              MOVE "UNABLE TO LOCATE DEALER RESERVE RECORD"
                                                       TO MESS
              PERFORM SEND-MESS
              GO TO EXIT-TEST-DLR-RESERVE.
           IF CHECK-PAYEE = "DR"
              IF CX-AMT > DS-DLRVPRIN(CX-DLRVNO)
      *          MOVE DS-DLRVPRIN(CX-DLRVNO) TO RSV-EDIT
                 MOVE "RSV" TO FORMAT-ALP1 FORMAT-ALP2 FORMAT-ALP3
                 MOVE 1 TO FORMAT-NO1
                 MOVE 2 TO FORMAT-NO2
                 MOVE 3 TO FORMAT-NO3
                 MOVE DS-DLRVPRIN(1) TO FORMAT-AMT1
                 MOVE DS-DLRVPRIN(2) TO FORMAT-AMT2
                 MOVE DS-DLRVPRIN(3) TO FORMAT-AMT3
                 MOVE " " TO MESS
                 STRING
                 "INSUFFICIENT PRIN: " FORMAT-DS-MSG
                         DELIMITED BY SIZE INTO MESS
                 PERFORM SEND-MESS
              ELSE
                 GO TO EXIT-TEST-DLR-RESERVE.
           IF CHECK-PAYEE = "DR"
              IF CX-DLRVNO = 3
                 IF DL-AFP-DEALER = "Y"
                    IF CX-AMT > DS-AFPDUEDL-MEMO
                       MOVE DS-AFPDUEDL-MEMO TO RSV-EDIT
                       MOVE " " TO MESS
                       STRING
                       "INSUFFICIENT AFP DUE DEALER: " RSV-EDIT
                           DELIMITED BY SIZE INTO MESS
                       PERFORM SEND-MESS
                    ELSE
                       GO TO EXIT-TEST-DLR-RESERVE.
           IF CHECK-PAYEE = "DC"
              IF CX-AMT > DS-DLRVCHGS(CX-DLRVNO)
      *          MOVE DS-DLRVCHGS(CX-DLRVNO) TO RSV-EDIT
                 MOVE "CHG" TO FORMAT-ALP1 FORMAT-ALP2 FORMAT-ALP3
                 MOVE 1 TO FORMAT-NO1
                 MOVE 2 TO FORMAT-NO2
                 MOVE 3 TO FORMAT-NO3
                 MOVE DS-DLRVCHGS(1) TO FORMAT-AMT1
                 MOVE DS-DLRVCHGS(2) TO FORMAT-AMT2
                 MOVE DS-DLRVCHGS(3) TO FORMAT-AMT3
                 MOVE " " TO MESS
                 STRING
      *          "INSUFFICIENT RESERVE FROM CHARGES BALANCE: "
      *                                           RSV-EDIT
                 "INSUFFICIENT CHGS: " FORMAT-DS-MSG
                         DELIMITED BY SIZE INTO MESS
                 PERFORM SEND-MESS
              ELSE
                 GO TO EXIT-TEST-DLR-RESERVE.
           IF CHECK-PAYEE = "DP"
              IF CX-AMT > DS-DLPARVHELD
                 MOVE DS-DLPARVHELD TO RSV-EDIT
                 MOVE " " TO MESS
                 STRING
                 "INSUFFICIENT PARTICIPATION RESERVE BALANCE: "
                                                  RSV-EDIT
                         DELIMITED BY SIZE INTO MESS
                 PERFORM SEND-MESS
              ELSE
                 GO TO EXIT-TEST-DLR-RESERVE.

      * IF THE BRANCH FLAG IS NOT 'Y' THEN THEY DONT WANT
      * TO BE ASKED THE QUESTION TO OVERRIDE
      * BR-FILE READ AFTER PROCED-DIV
           IF BR-NEGDLRSV NOT = "Y"
              MOVE "Y" TO RSV-NEGATIVE
              GO TO EXIT-TEST-DLR-RESERVE.
           MOVE SPACES TO MESS.
           PERFORM SEND-LEGEND.
       TEST-DLR-QUESTION.
           MOVE "ACCEPT NEGATIVE RESERVE AMOUNT (Y/N) ?" TO MESS.
           PERFORM SEND-LEGEND.

           MOVE "E RA060SEL." TO VDU.
           PERFORM SCREENIO.
           IF VDUBUF = "     Y"
              GO TO EXIT-TEST-DLR-RESERVE.
           IF VDUBUF NOT = "     N"
              GO TO TEST-DLR-QUESTION.
           MOVE "Y" TO RSV-NEGATIVE.
       EXIT-TEST-DLR-RESERVE.
           MOVE SPACES TO MESS.
           PERFORM SEND-LEGEND.

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
              MOVE "WARNING, UNABLE TO LOCATE DEALER STATS"
                                          TO MESS
              PERFORM SEND-MESS
              MOVE "DS STATS?" TO D-09DESC
              MOVE SPACES TO D-09B D-09C
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
      *       COMMON UPDATE ROUTINES WITH ALL CHECK PRINTS
      *
       COPY "LIBLP/CHECKTRANS.CPY".
      *---------------------------------------------------------
       COPY "LIBGB/GPENV.CPY".
       COPY "LIBGB/SCREEN.CPY".
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
      **************************************************************************
      *  REQUIRED SECTION: CALLED BY SCREENIO:
      *
      *  RESET FORM (SCREEN) TO ITS INITIAL STATE (LIKE FRESET).
      **************************************************************************
       FORM-RESET SECTION.
           DISPLAY CHCKI-SCREEN UPON TCLP-MAIN-WINDOW-HANDLE.
           MOVE EXT-CONAME-SYSDATE TO CHCKI-SYSDATE.
           MOVE CHCKI-FORM-FIELDS TO WR-BUF.
           MOVE CHCKI-FORM-LIST   TO WR-LIST.
           PERFORM CALL-WRFORM.
           
       FORM-RESET-EXIT.
           EXIT.

      **************************************************************************
      * REQUIRED SECTION: CALLED BY SCREENIO:
      *
      * SETUP CORRECT HELP FIELDS
      **************************************************************************
       SETUP-LINK-INFO SECTION.
           IF VDU-FORM-NAME = "LPSMNU"
              MOVE LPSMNU-HELP--FILE TO HELP-LINK-FILE
              MOVE LPSMNU-HELP--DIR  TO HELP-LINK-DIR
           ELSE
              MOVE CHCKI-HELP--FILE TO HELP-LINK-FILE
              MOVE CHCKI-HELP--DIR TO HELP-LINK-DIR.

       VDU-CONVERT-FIELD SECTION.
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBLP/CHCKI_EVA.CPY".
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

       SEND-REJECT-MESS.
           IF DT-STATUS NOT = " "
              MOVE "DETAIL STATUS OF '&' INVALID FOR CHECK WRITING "
                 TO MESS
              INSPECT MESS REPLACING FIRST "&" BY DT-STATUS.
           IF DT-STATUS = "R"
             IF DT-PAID = "B"
                MOVE "CHECK RELEASE REQUESTED IN BATCH PROCESSING"
                TO MESS
                PERFORM SEND-MESS
             ELSE
                MOVE "CHECK RELEASE REQUESTED IN LOAN ENTRY"
                TO MESS
                PERFORM SEND-MESS
             END-IF
             MOVE "USE F6 SCAN AT `PAYEE TYPE' TO PROCESS REQUEST"
             TO MESS.

           IF DT-STATUS = "P"
              MOVE "CHECK REQUEST WAS PROCESSED" TO MESS.

           PERFORM SEND-MESS.

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
       SETUP-CX-FROM-DT.
           MOVE DT-BRNO       TO CX-BRNO.
           MOVE DT-CLASS      TO CX-CLASS.
           MOVE DT-ACCTNO     TO CX-ACCTNO.
           MOVE CX-ACCTNO     TO CX-REF.
           MOVE DT-DLNO       TO CX-DLNO DL-DLNO.
           MOVE DT-DATE       TO CX-DT-DATE.
           MOVE DT-SEQ        TO CX-DT-SEQ.
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
           IF CX-TYPE = "VX"
              STRING "DL:" D-0101 " CL-"
                 D-020 "-" D-021 DELIMITED BY SIZE INTO MESS
           ELSE
              STRING CX-TYPE ":" D-0101 " CL-"
                 D-020 "-" D-021 DELIMITED BY SIZE INTO MESS.
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
           MOVE CX-REF    TO L-NUMBER.
           MOVE PAYEE-SUB TO L-PAYEE-SUB.
           MOVE L-ACCTNO  TO LOAN-KEY.
       NOT-FOUND.
           MOVE "NOT FOUND" TO MESS.
           PERFORM SEND-MESS.
       RESET-SCREEN.
           MOVE SPACES TO WR-BUF.
           MOVE DISPLAY-HEAD-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
           MOVE SPACES TO WR-BUF.           
           MOVE DISPLAY-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
      * CLEAR SCAN KEY AND CHECK #
           MOVE SPACES TO WR-BUF.
           MOVE "FMKEY K030." TO WR-LIST.
           PERFORM CALL-WRFORM.
           MOVE SPACES TO WR-BUF.
           MOVE "TILL GLBAL." TO WR-LIST.
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
           MOVE "S  A000NEW." TO VDU.
           MOVE SPACES TO VDUBUF.
           PERFORM SCREENIO.

       DISPLAY-SCREEN-HEADER.
           MOVE CX-PAID TO TRANS-TYPE.
           PERFORM VERIFY-TRANS-TYPE.

           MOVE CX-TYPE TO VDUBUF CHECK-PAYEE.
           PERFORM VERIFY-CHECK-PAYEE.

           MOVE CX-DLRVNO TO DLRVNO.

           MOVE DISPLAY-HEAD-BUF TO WR-BUF.
           MOVE DISPLAY-HEAD-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       SEND-PRU-FORM.
           MOVE "S  A020PRU." TO VDU.
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
           MOVE "S  A060SEL." TO VDU.
           MOVE SPACES TO VDUBUF.
           PERFORM SCREENIO.
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
           IF CX-PGM = "B2" OR
      * IF DEALER BATCH CHECK, AND YOU ARE NOW PRINTING IT MANUALLY,
      * IT WAS ALLOWING YOU TO CHANGE THE AMOUNT AS IF YOU WERE A
      * MANUAL CHECK FROM SCRATCH !!
               ( P-CODE(PAYEE-SUB) = "DL" AND CXFILE-SCAN-FLAG = "Y")
              MOVE SALES-CHAN-TABLE TO CHFG-TABLE.

           IF P-CODE(PAYEE-SUB) = "LN"
              IF CX-PGM = "D0"
                 MOVE DIRECT-CHAN-TABLE TO CHFG-TABLE
              ELSE
      *  JTG 990706 ADDED LOGIC FOR REVOLVING DIRECT LOAN ADVANCES 'AV'
      *             VIA: LONPFM.C
                 IF CX-PGM = "F2" OR "FM"
                    MOVE TRANS-CHAN-TABLE TO CHFG-TABLE.
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
      * SHOULD COME BACK WITH IP-AOK SET TO "Y" FROM LONPMC
           MOVE FD-PROG TO IP-PROG.
           IF PRINT-PROG NOT = " "
              MOVE PRINT-PROG TO IP-PROG.
           MOVE "Y" TO IP-AOK.


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
       COPY "LIBLP/LPBW1RN.CPY".
       COPY "LIBLP/LPCA1IN.CPY".
       COPY "LIBLP/LPCD1RN.CPY".
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
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY CHCKI-SCREEN.
       STOP-RUN.
           EXIT PROGRAM.
