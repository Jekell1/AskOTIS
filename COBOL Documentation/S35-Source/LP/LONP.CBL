      * A30 CONVERSION COMPLETED
       IDENTIFICATION DIVISION.
       PROGRAM-ID.      LONP.
      *****************************************************
      *  DIR :  LP
      *  DESC:  LOAN PROCESSING DRIVER
      *  SCREEN - PROGRAM - DESC
      *  LONPA    LONPA0    ENTER TRANSACTION DATE
      *  LONPB    LONPB0    SALES FINANCE NEW LOAN ENTRY
      *           LONPB2    S.F. CONTRACT UPDATE
      *  LONPC    LONPC0    DIRECT LOANS NEW LOAN ENTRY - PAGE 1
      *           LONPC2    DIRECT LOANS CONTRACT UPDATE
      *  XONPC    XONPC0    DIRECT LOAN NEW ENTRY W/FRONT END CALC - PG1
      *  ZONPC    ZONPC0    DIRECT LOAN WITH BALLOONS
      *  LONPD    LONPD0    DIRECT LOANS NEW LOAN ENTRY - PAGE 2
      *  LONPF    LONPF0    TRANSACTION POSTING - TRNASACTION HEADER
      *           LONPF1    TRANSACTION POSTING DRIVER
      *           LONPF2    TRANSACTION UPDATE
      *           LONPF3    TRANSACTION CHGS AND TRCD DISPLAY
      *           LONPF5    TRANSACTION UPDATE, TRCD "TS" ONLY
      *           LONPF7    TRCD'S  AL,AI,AP,WL,WI,WO,OT,O2,W2,OP,RE
      *           LONPF8    TRCD'S  RV
      *           LONPF9    TRCD'S  JD,PL,PI,PW,P2,P3,P4,P5,RP,IR,AC,BK,BD
      *           LONPFA    TRCD'S  RN,SC,PB,PO,RB,RO
      *           LONPFB    TRCD'S  RC,RA,RP,R1,R2,R3,RI,RS,RM,RD,CC,CA,
      *                             CP,C1,C2,C3,CD
      *           LONPFC    TRCD'S  PY,PA,PE,PR,PC,PP,Z2,SS,AH,DF(1-9),PX,PN,PZ
      *           LONPFD    TRCD'S  TS,TP
      *           LONPFE    TRCD'S  SP,DC,AD(1-9),SE,SX,SV
      *           LONPFF    TRCD'S  REVERSAL FOR ADDONS, ADJUST, CANCELS
      *           LONPFG    TRCD'S  LR  LOAN RESCHEDULE
      *           LONPFL    TRCD'S  AV,      LOAN REVOLVING ADVANCES
      *           LONPFM    TRCD'S  AV,      LOAN REVOLVING ADVANCES
      *           LONPFR    TRCD'S  PU,      S.F. REVOLVING ADDONS
      *           LONPFS    TRCD'S  PU,      S.F. REVOLVING ADDONS
      *           LONPFZ    TRANSACTION UPDATE, TRCD "TP" ONLY
      *  LONPG    LONPG0    ADDON DRIVER
      *           LONPG3    ADDON FEE AND INSUR ENTRY CALC.
      *           LONPG4    ADDON INSUR UPDATE
      *           LONPG5    ADDON INSUR ADJUSTMENT AND CALC
      *           LONPG6    ADDON ADJUSTMENT UPDATE
      *           LONPG7    ADDON INSUR CANCEL ENTRY AND CALC
      *           LONPG8    ADDON CANCEL UPDATE
      *  LONPH    LONPH0    TRANSACTION RECAP INQUIRY
      *  LONPI    LONPI0    CASH DRAWER RECEIPTS AND DEALER RESERVE RECEIPTS
      *  DISBI    DISBI0    CASH DISBURSEMENTS (WAS 1ONPI0)
      *  CHCKI    CHCKI0    CHECK WRITING (WAS 2ONPI0)
      *  VOIDI    VOIDI0    VOID CHECKS (WAS 3ONPI0)
      *  BNKDI    BNKDI0    BANK DEPOSITS (WAS 4ONPI0)
      *  BNKWI    BNKWI0    BANK WITHDRAWALS (WAS 5ONPI0)
      *  BCHKI    BCHKI0    BATCH CHECK PRINT (WAS 6ONPI0)
      *  LONPJ    LONPJ0    SALES FINANCE LOAN INQUIRY/MODIFY PAGE 1
      *           LONPJ1    SALES FINANCE VERIFY CONTRACT DELETE
      *           LONPJ2    SALES FINANCE CONTRACT DELETE
      *  LONPK    LONPK0    SALES FINANCE LOAN INQUIRE/MODIFY PAGE 2
      *  LONPL    LONPL0    OPEN/CLOSE CASH TILL
      *  LONPM    LONPM0    FORM PRINT DRIVER
      *           LONPM2    CHECK PRINT
      *           LONPM3    COUPON PRINT
      *           LONPM4    PAYMENT RECEIPT PRINT
      *           LONPM5    LEDGER CARD (INQUIRY) PRINT
      *           LONPM7    LOAN CONTRACT PRINT
      *           LONPM8    BATCH PRINT LEDGERS (TENNESSEE)
      *           LONPM9    CUSTOMER STATEMENTS
      *           LONPMB    VARIABLE FORM TEST PRINT
      *           LONPMC    VARIABLE FORM PRINT
      *           LONPMI    BATCH LEDGER HEAD UP
      *           LONPMQ    PARADATA STANDARD BATCH PRINT LEDGERS
      *           LONPMR    PARADATA STANDARD BATCH PRINT LEDGERS HEAD
      *  LONPN    LONPN0    DIRECT LOAN - MOD/INQ
      *           LONPN1    VERIFY DELETE
      *           LONPN2    DIRECT LOAN DELETE
      *  XONPN    XONPN0    FRONT END DIRECT LOAN - MOD/INQ
      *  LONPO    LONPO0    DIRECT LOAN MOD/INQ PAGE 2
      *                     5/11/2023 ARCHIVED LONPP0, REMOVED
      *  LONPP    LONPP0    LOAN/TRANSACTION INQUIRY - LEDGER CARD PRINT
      *  LONPQ    LONPQ0    RESCISSION DRIVER
      *           LONPQ1    RESCISSION UPDATE/DELETE/INQUIRY
      *  LONPR    LONPR0    CLOSING: BALANCE PROCEDURES
      *  LONPS    LONPS3    LOAN INQUIRY - VARIABLE INQUIRY
      *           LONPS4    LOAN INQUIRY - LOAN CHGS & FORM REQUESTS
      *  LONPT    LONPT0    DEALER BATCH CHECK RELEASE
      *  LONPU    LONPU0    SALES FIN - MOD/INQ PAGE 3
      *  LONPV    LONPV0    DIRECT LOAN - MOD/INQ PAGE 3
      *           CALCZL    DIRECT LOAN FRONT END CALCULATOR
      *           CALCZ3    SALES FIN FRONT END CALCULATOR
      *  LONPW    LONPW0    BATCH PAYMENT MENU
      *           LONPW1    ESTABLISH BATCH
      *           LONPW9    BATCH PAYMENT REFERENCE CODE PROCESSING
      *           LONPWA    BATCH PAYMENT OTHER CHG REFERENCE CODE PROCESSING
      *  LONPX    LONPX0    CHANGES MENU
      *  LONPY    LONPY0    COLLETERAL ID INQUIRY
      *
      *
      *           XONIN0    DIRECT LOAN CONVERSION - PG1
      *
      *
      *---------------------SHARED WITH OTHER MODULES----------
      *  PROGRAMS WITHIN THIS HEADING ARE NOT ASSOCIATED WITH A
      *  SPECIFIC SCREEN
      *--------------------------------------------------------
      *           LONPP0    LEDGER CARD PRINT
      *--------------------------------------------------------
      *  SELECTION   RELATIVE WORK RECORD
      *
      *  LONPB0      1
      *  LONPC0      2
      *  LONPF0      3
      *  LONPD0      4
      *  LONPJ0      5
      *  LONPK0      6
      *  LONPN0      7
      *  LONPO0      8
      *  LONPL0      9
      * REV:
      *  021494 BAH ADDED ROTINE TO REMOVE WORK FILE
      *  101597 JTG ADDED OPTIONAL DIRECT LOAN SUBMENU, RE: BR-BALLOON-LOANS-FG
      *  103097 UPDATE LIST ABOVE
      *  980928 BLV ADD ACCESS CHECK ON OPFILE BEFORE OPEN; PEOPLE WERE COMING
      *             BACK FROM PA0 VIA DECLARATIVES & GETTING OPEN ERROR OF 35:
      *             USER 1 IN LONPR0 AT ACCEPT DEPOSIT; USER 2 GOES INTO
      *             PROCESSSING, GETS "SOMEONE IS UPDATING", THEN OPEN OF 35
      * JTG 101298 ADDED SUB MENU FOR REVOLVING RE: BR-REVOLVING-SF-FG
      * JTG 990610 ADDED SUB MENU FOR REVOLVING RE: BR-REVOLVING-LN-FG
      * GLB 001103 ADDED 7. 6ONPI FOR BATCH CHECK PRINTING
      * BAH 050719 ADDED TEST FOR BR-STATE = "MX" AND CERTAIN PROGRAMS, TO
      *            GET SPANISH FORMS FROM "SM" DIRECTORY INSTEAD OF "FM"
      *             DIRECTORY, WORLD PR# 414
      * BLF 051013 TEST FOR ERRCD OF "Z", SET BY DECLARATIVES IF FILE
      *            STATUS ERROR OF 98, IF ERRCD IS "Z", MOVE SPACES TO
      *            FORM-NAM SO TCLP WILL LOG USER OFF SYSTEM
      * BLF 051013 ADDED REDEFINITION OF FORM-PATHNAME FOR TEST IN
      *            DECLARATIVES
      * BLF 051021 IF ERRCD OF "Z" (FILE STATUS 98), CREATE A LOCK FILE:
      *            LOCKLP IN LOCAL ED.  CHECK FOR LOCKLP EACH TIME BEFORE A
      *            PROGRAM IS CALLED - IF LOCK IS FOUND, SEND MESSAGE & GET
      *            OUT.  DON'T LOG USER OUT ANYMORE SINCE THEY CAN'T GET
      *            INTO DAILY PROCESSING.  PR# 2549.
      * BLM 101129 ADD GPENV, IF NEW SECURITY (GP-SECURITY-FG = Y), DO
      *            NEW SECURITY CHECKS, PR# NEWSECURITY
      * BLM 111130 ACTIVATE NEW SECURITY ROUTINES IN A15, PR# NEWSECURITY
      * BLM 130222 CHANGE "CALL ERRMSG" TO PERFORM CALL-ERRMSG
      * BLM 130307 ADD GETFM TO CHECK SECURITY
      * BAH 140502 RENAMED 1ONPI0-6ONPI0 TO NOT START WITH A NUMBER, VERYANT
      * BAH 170313 OPEN-PATH MUST KEEP MMDDYY DATE
      * BAH 20190625 REMOVED BR-REVOLVING-SF-FG BR-REVOLVING-LN-FG PT0153
      * JKC 2021-0908 UNCOMMENTED THE 'PERFORM CLOSE-OP-FILE' IN
      *               END-ROUTINE; NOT SURE WHO OR WHY COMMENTED IT OUT
      *               BUT JKC & BAH HIT AN ERROR AND FOUND THIS.
      *
      * JKC 2024-0201 ADDED OTIS TRACE LOGGING LOGIC
      *
      *************************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "LIBLP/LPFSOP.CPY".
       DATA DIVISION.
       FILE SECTION.
       COPY "LIBLP/LPFDOP.CPY".
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)./LP/LONP.CBL S35 07/01/24-12:23:06 >".
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       01  OP-IFN                      PIC X(2) VALUE "OP".

       01  FIRST-TIME        PIC X     VALUE "Y".
       01  PROG-WK.
           03  FILLER        PIC X(7)  VALUE "LP/LONP".
           03  PROG-NO       PIC XX    VALUE "A0".

       01  PROG-WKX REDEFINES PROG-WK.
           03  FILLER                 PIC X(3).
           03  PROG-WKX-PREFIX-NO.
               05  PROG-WKX-PREFIX    PIC X(4).
               05  PROG-WKX-NO        PIC XX.

      *-----------------------------------------------------------------
      *    /USR/DATA/R#/SSBBBB/ED/LOCKLP
      *-----------------------------------------------------------------
       01  LOCK-PATH.
           03  LOCK-PATH-BASE           PIC X(09).
           03  FILLER                   REDEFINES LOCK-PATH-BASE.
               05  LOCK-PATH-BASE-USR   PIC X(05).
               05  LOCK-PATH-BASE-DIR   PIC X(04).
           03  FILLER                   PIC X(01)   VALUE "/".
           03  LOCK-PATH-MACHINE        PIC X(02).
           03  FILLER                   PIC X(01)   VALUE "/".
           03  LOCK-PATH-DATA           PIC X(06).
           03  FILLER                   PIC X(10)   VALUE "/ED/LOCKLP".

       COPY "LIBGB/DATERW.CPY".
       COPY "LIBLP/LPWKPATH.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBGB/LOADCOW.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBLP/LPLOANW.CPY".
       COPY "LIBLP/LPPFBUF.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBLP/LPEDPATH.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".

       LINKAGE SECTION.
       01  FORM-PATHNAME      PIC X(22).
       01  EXIT-PATHNAME      PIC X(22).

       PROCEDURE DIVISION
             USING FORM-PATHNAME EXIT-PATHNAME.
       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
              OP-FILE.
       COPY "LIBGB/DECLARE.CPY".
           PERFORM CLOSE-BR-FILE.
           CLOSE OP-FILE.
       COPY "LIBGB/DECLAR2.CPY".

      **********************************
       MAIN-PROGRAM SECTION.
      **********************************
      D    STOP MESS.

           PERFORM SQL-CONNECT.

      * PRE-LOAD OPTIONS (DEBUG MODE, ETC)
           MOVE "OPT" TO GETENV-BUF.
           PERFORM GETENV-CALL.
           IF STAT = "00"
              MOVE GETENV-BUF TO LOADCO-OPT.

           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "LP/LONPA" TO FORM-NAM.
           MOVE FORM-PATH TO FORM-PATHNAME.

       COPY "LIBGB/CHKSEC.CPY".

      * MAKE SURE INITIAL NON DISPLAY BUFFERS CLEARED/SETUP PROPERLY

      * THESE WERE NON DISPLAY BUFFERS, NOW THEY WILL GET PASSED TO ALL
      * LOAN PROCESSING CALLED FROM HERE (LONP.C)
           MOVE SPACES TO PROG-BUF UPDATE-BUF.
           MOVE "F" TO FULL-DISPLAY.
           MOVE EXT-CONAME-SYSDATE TO LPLOAN-SYSDATE.
           MOVE EXT-CONAME-USER-ID TO LPLOAN-USERID.
           MOVE 0 TO TRANS-DATE LOAN-KEY BORROWER-KEY BATCH TRANS-CODES.

           MOVE "A0A" TO NEW-PROG-FORM.

           GO TO GET-PROG.

       GET-FORM.
           MOVE NEW-FORM TO PROG-NO.

           MOVE PROG-WK TO FORM-NAM.

      ***********************************************
      *   S E C U R I T Y   C H E C K:
      *   IF STAT = "3U" UPDATE FLAG WAS OFF
      *             "3O" UPDATE FLAG WAS ON
      *                  OWNERSHIP FLAG WAS OFF
      *             "3S" UPDATE FLAG WAS ON
      *                  OWNERSHIP FLAG WAS ON
      *                  USER PROFILE FLAG WAS OFF
      ***********************************************

           MOVE FORM-PATH TO CHKSEC-FORM-PATH.
           MOVE "CO"      TO FORM-OBJ.
           MOVE "CL/CHKSEC" TO FORM-NAM.
           CALL "CHKSEC" USING CHKSEC-FIELDS ON OVERFLOW 
                MOVE CHKSEC-FORM-PATH TO FORM-PATH
                MOVE "3Z" TO FORM-OBJ
                GO TO EXIT-PROG
           END-CALL.
           CANCEL "CHKSEC".

           MOVE CHKSEC-STAT TO STAT.
           IF STAT NOT = "00"
              MOVE STAT TO FORM-OBJ
              GO TO EXIT-PROG.

       GET-PROG.
           MOVE EXT-FILPATH-BASE    TO LOCK-PATH-BASE.
           MOVE EXT-FILPATH-MACHINE TO LOCK-PATH-MACHINE.
           MOVE EXT-FILPATH-DATA    TO LOCK-PATH-DATA.
           MOVE LOCK-PATH           TO ACCESS-BUF.
           PERFORM ACCESS-CALL
           IF STAT = "00"
              PERFORM ERR-98-LOCK
              GO TO END-ROUTINE.

           MOVE NEW-PROG TO PROG-NO.
           MOVE PROG-WK TO FORM-NAM.
       COPY "LIBGB/LOADCOLP.CPY".
       LOAD-POST.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    NEED TO SET EXT-TCLP-TRACE-FORM-PATHNAME BEFORE GOING INTO
      *    'TRACE-PROGRAM-RETURN' SO THAT IT CAN GET THE EXACT
      *    PROGRAM NAME IN THE TRACE LOG
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE FORM-PATHNAME TO EXT-TCLP-TRACE-FORM-PATH.
           MOVE "LONP"        TO EXT-TCLP-TRACE-FORM-PROGX.
           PERFORM TRACE-PROGRAM-RETURN.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ERRCD = "X"
              GO TO END-ROUTINE.
           IF ERRCD = "E"
              PERFORM ERR-PROG
              GO TO END-ROUTINE.
           IF ERRCD = "Z"
              STRING ">" LOCK-PATH DELIMITED BY SIZE INTO SYSTEM-BUF
              PERFORM SYSTEM-CALL
              PERFORM ERR-98-LOCK
              GO TO END-ROUTINE.

      * 04/04/2020 CS & BH, THIS CODE MUST EXIST! DO NOT COMMENT OUT, WE DONT
      *            FULLY UNDERSTAND IT, BUT IF COMMENTED OUT YOU WILL BE ABLE
      *            TO CLOSE THE DAY WITH PEOPLE IN. 
      * IT'S POSSIBLE TO RETURN FROM LONPA0 VIA DECLARATIVES WITH STAT
      * 93 - IF SO, THEN CHECK ACCESS ON OP & EXIT IF BAD. BLV 9/28/98
           IF FIRST-TIME = "Y"
              MOVE BRANCH     TO OPEN-PATH-BRNO
              MOVE TRANS-DATE TO DATE-YYYYMMDD
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
              MOVE DATE-MMDDYY TO OPEN-PATH-TRDATE
              PERFORM LOAD-OPEN-FILE
              MOVE OPEN-PATH TO ACCESS-BUF
              PERFORM ACCESS-CALL
              IF STAT = "00"
                 PERFORM OPEN-OP-FILE
                 MOVE "N" TO FIRST-TIME
              ELSE
                 GO TO END-ROUTINE.

           IF NEW-PROG-FORM = "R0R"
              PERFORM CLOSE-OP-FILE.

           IF PROG-WKX-NO = "A0"
              MOVE FORM-PATHNAME TO EXT-TCLP-TRACE-FORM-PATH
              MOVE "LONP"        TO EXT-TCLP-TRACE-FORM-PROGX
              MOVE BRANCH TO BR-NO
              PERFORM OPEN-BR-FILE
              PERFORM READ-BR-FILE
              PERFORM CLOSE-BR-FILE
              IF IO-FG = 9
                 MOVE "BRANCH RECORD CANNOT BE ACCESSED" TO MESS
                 PERFORM SEND-MESS
                 GO TO END-ROUTINE.

           IF ERRCD = "P"
              MOVE NEW-PROG TO PROG-WKX-NO
              PERFORM SELECT-VERSION
              GO TO GET-PROG.

           MOVE NEW-FORM TO PROG-WKX-NO.
           PERFORM SELECT-VERSION.
           GO TO GET-FORM.

      *******************************************
       MISC SECTION.
      *******************************************
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/SYSTEM.CPY".
       COPY "LIBGB/GETENV.CPY".
       COPY "LIBGB/RMFILE.CPY".
       FORM-RESET.
       SETUP-LINK-INFO.
       VDU-CONVERT-FIELD.
       SELECT-VERSION.
           MOVE "LONP" TO PROG-WKX-PREFIX.
           IF BR-FRONT-END-FG = "Y"
              IF (PROG-WKX-NO = "C " OR "C0") OR
                   (PROG-WKX-NO = "N " OR "N0")
                 MOVE "XONP" TO PROG-WKX-PREFIX
                 IF PROG-WKX-NO = "C " OR "C0"
                    IF BR-BALLOON-LOANS-FG = "Y"
                       IF TRANS-CODES = 2
                          MOVE "ZONP" TO PROG-WKX-PREFIX.


           IF BR-FRONT-END-FG = "Y"
              IF BR-INS-ADDON-ORIG
                 IF PROG-WKX-NO = "C3" OR "C5" OR "C7"
                    MOVE "YONP" TO PROG-WKX-PREFIX.
           IF PROG-WKX-NO = "I0" OR "I "
      * 2. CASH DISBURSEMENT
              IF TRANS-CODES = 1
                 MOVE "DISB" TO PROG-WKX-PREFIX
              ELSE
      * 3. CHECK WRITING
              IF TRANS-CODES = 2
                 MOVE "CHCK" TO PROG-WKX-PREFIX
              ELSE
      * 4. VOID
              IF TRANS-CODES = 3
                 MOVE "VOID" TO PROG-WKX-PREFIX
              ELSE
      * 5. BANK DEPOSITS
              IF TRANS-CODES = 4
                 MOVE "BNKD" TO PROG-WKX-PREFIX
              ELSE
      * 6. BANK WITHDRAWALS
              IF TRANS-CODES = 5
                 MOVE "BNKW" TO PROG-WKX-PREFIX
              ELSE
      * 7. BATCH CHECK PRINTING
              IF TRANS-CODES = 6
                 MOVE "BCHK" TO PROG-WKX-PREFIX.
           IF PROG-WKX-NO = "E0" OR "E "
              IF ERRCD = "2"
                 MOVE "EONP" TO PROG-WKX-PREFIX.
       ERR-PROG.
           MOVE "ERROR IN LOAN PROCESSING" TO MESS
           PERFORM SEND-MESS.
           DESTROY ALL CONTROLS.
       ERR-98-LOCK.
           MOVE "  WARNING!!!" TO ERRMSG-HEAD.
           MOVE "      A CORRUPTED FILE SITUATION HAS CAUSED YOUR"
              TO ERRMSG-MSG1.
           MOVE "      BRANCH TO BE LOCKED OUT OF DAILY PROCESSING!"
              TO ERRMSG-MSG2.
           PERFORM CALL-ERRMSG.

           MOVE "IMPORTANT!!!" TO ERRMSG-HEAD.
           MOVE "      PLEASE CALL YOUR ADMINISTRATOR OR SUPPORT REP"
              TO ERRMSG-MSG1.
           MOVE "      IMMEDIATELY FOR FILE REPAIR AND RECOVERY!!"
              TO ERRMSG-MSG2.
           PERFORM CALL-ERRMSG.
           DESTROY ALL CONTROLS.

       COPY "LIBCL/TRACE.CPY".
       COPY "LIBGB/DATER.CPY".

      ************************************************
       IO-ROUTINE SECTION.
      ************************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBLP/LPEDPATHIO.CPY".
       OPEN-OP-FILE.
           PERFORM OPEN-IT.
           MOVE OPEN-PATH TO E-FILE.
           OPEN I-O OP-FILE.
           IF IO-FG = 8
              GO TO OPEN-OP-FILE.
           IF IO-FG = 7
              PERFORM CLOSE-OP-FILE
              GO TO OPEN-OP-FILE.
       CLOSE-OP-FILE.
           MOVE "CLOSE" TO E-MSG.
           CLOSE OP-FILE.

      ************************************************
       END-ROUTINE SECTION.
      ************************************************
       EXIT-PROG.

           IF USER-PID NOT = " "
              MOVE USER-PID TO TEMP-PID
              MOVE TEMP-PATH TO ACCESS-BUF
              PERFORM REMOVE-WORKFILE.

      * I PUT THIS DESTROY HERE BECAUSE I HIT A MISSING STATE PARAMETER
      * IN LONPFE  AND THAT PASSES
      * BACK ERRCD "X", WHICH SEND TO END-ROUTINE, WHICH IS HERE,
      * AND MY SCREEN CAME BACK ALL MESSED UP

           DESTROY ALL CONTROLS.

       LOAD-RESTART.
           PERFORM CLOSE-OP-FILE.
       STOP-RUN.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    NEED TO SET EXT-TCLP-TRACE-FORM-PATHNAME BEFORE GOING INTO
      *    'TRACE-PROGRAM-FINISH' SO THAT IT CAN GET THE EXACT
      *    PROGRAM NAME IN THE TRACE LOG
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE FORM-PATHNAME TO EXT-TCLP-TRACE-FORM-PATH.
           MOVE "LONP"        TO EXT-TCLP-TRACE-FORM-PROGX.
           PERFORM TRACE-PROGRAM-FINISH-STOP.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           EXIT PROGRAM.
