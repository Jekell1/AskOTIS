       IDENTIFICATION DIVISION.
       PROGRAM-ID.      WIMENU.
       DATE-WRITTEN.    082989.
      ******************************************************************
      *
      *  DESC:  WINDOWS MAIN SELECTION MENU
      *         F10 INTERRUPT 
      *
      * REV:
      *  MJD 092195 ADDED OPTION 8 INTER BRANCH PAYMENTS.
      *  JTG 101795 PASSWORD PROTECTED INTER BRANCH PAYMENTS
      *  BAH 011796 ADDED APPROVAL LIMIT, E022
      *  JTG 041597 ADDED LOAN APPLICATIONS OPTION
      *  JTG 081497 LOCKED OUT GIFTNET FOR LNQUOT
      *  KEC 082797 EXPANDED THE WINDOW;
      *              CHANGED SO TO DISPLAY OPTIONS IF FLAGGED 'ON';
      *              ADDED GIFTNET 'DIRECT LINK' OPTION;
      *              ADDED 'SET PIGGYBACK PRINTER' OPTION.
      *  GLF 091897 REPLACED ANY PROMPT THAT DISPLAYS WORDS
      *             'DIRECT LINK' W/'WCBC'
      *  JTG 092597 ADDED BLQUOT.C (BALLOON LOAN QUOTER)
      *  JTG 121297  THIS CALL TO SETENV DONE IN GB/TCLP.C, AND DONE
      *              HERE ON F10 INTERRUPT TO PROVIDE A MEANS OF
      *              REFRESHING THE ENVIRONMENT.
      *  MG  991004 FIXED VDUSTATUS/IO-FG/STAT PER DEVELOPMENT MEETING.
      *  MJD 000131 ADDED BBINQ.C (BULLETIN BOARD)
      *  BLV 030422 ADDED EMPLOYEE TIME TICKET ENTRY, CASHCO PR# 2045
      *  BAH 050729 FROM 6/22/05 UNTIL 7/29/05, MALBOX WAS NOT REFRESHING
      *             THE F10 WINDOW...OOPS
      *  BLF 051219 CHANGED TO CALL SIMENU WITH WIMENU-SELECTION INSTEAD
      *             OF VDU-WIMENU-SELECTION, WAS ALLOWING INTERBRANCH
      *             IN PAYMENT POSTING
      *  BLF 070504 IF GP-I-AM-WORLD, CALL TIMESH INSTEAD OF CLOCK0 FOR
      *             EMPLOYEE TIME SHEET ENTRY; CHANGE MENU DESCRIPTION
      *             FROM "TIME CARD" TO "TIME SHEET", WORLD PR# 488
      *  BAH 080513 IF GP-I-AM-WISC (RIVERSIDE) ADDED CHECK WRITING
      *             MADE IT FOR THEM ONLY UNTIL TESTED REAL GOOD !!!!
      *                                                     PR# 3192
      *  BAH 081016 NEEDED TO A NEW WIMENU-SELECTION THAT DOES NOT
      *             HAVE CHECK WRITING IN IT TO USE FOR F10 FROM
      *             CHECK WRITING, DONT ALLOW F10 AGAIN INTO ITSELF.
      *  BAH 090526 WIMENU-SELECTION "2" WAS BEEPING NON STOP AT WORLD
      *             IT HAD NO WHERE TO GO, SO ADDED 'OR "2"' TO ONE OF
      *             THE DISPLAY IFS..(BARB FOUND THIS)
      *  BAH 101029 RENAMED THE DISPLAY SECTIONS, FOUND IT CONFUSING.
      *  KEC 110111 {PR#00693} WORLD <A15> [JIM ROSENAUER]
      *             ADDED OPTION TO ACCESS THE SPANISH LOAN APPLICATION
      *             (LA/SIPGRM) IF IT IS A SPANISH BRANCH OTHERWISE
      *             USE THE ENGLISH LOAN APPLICATION (LA/LAMENU).
      *   CS 110301 IF GP-USE-ADDRESS-BW-SCAN, THE BORROWER ADDRESS SCAN
      *             IS AVAILABLE FROM F10   MODERN PR#3720
      *  MJD 130522 CONVERTED FOR A30 AND REPLACED FLDSEL WITH CALL-FLDSEL.
      *  BAH 130930 ADDED BR-WIMENU-CHECK-WRITING INSTEAD OF TESTING
      *             SPECIFIC CUSTOMERS TO ALLOW CHECK WRITING, BAY PR#4379
      *  MJD 131028 ADDED TEMP-WINDOW-OPEN/TEMP-WINDOW-CLOSE SECTIONS
      *             TO FIX PROBLEM WITH CALLING MENU'S (SEE MORE DETAILED
      *             DESCRIPTION WITH SECTION HEADER)
      *  BAH 140512 ADDED EXT-SPANISH-FORMS
      *  MJD 140603 ADDED EXT WINDOW CONFIG OPTIONS.
      *  MJD 140606 ADDED EXIT-PATHNAME TO SIMENU CALL
      *  BLM 140818 ADDED EXIT-PATHNAME TO INBRTR CALL
      *  MJD 140926 ADDED CLMENU_HLD.CPY.  WORKER USED TO HOLD EXT-WI-CL-WORKER
      *             SO VALUES CAN BE RESTORED AFTER CALL
      *  MJD 141009 FIXED BUG.    WHEN CALLING ADDRESS SCAN IT WAS STILL
      *             PERFORMING SETUP-SCREEN AND IT NO LONGER NEEDS TO DO THAT.
      *  BLM 141016 CALL XPINQ USING BWLNS3-LINK-BUF, WHEN XPINQ IS CALLED
      *             FROM BWLNS3 PASSING ACCT# & FLAG
      *   CS 141017 CHANGED ABOVE TO PASS BWLNS3-SCANYN AS 'N'. I WAS IN
      *             COLLECTIONS, HIT F10, F4 GLOBAL, SELECTED A BRANCH 10
      *             ACCT AND THE OPTION TO SEE LOANS F3-LOANS FOR THE
      *             BORROWER WAS MISSING BECAUSE THIS FLAG WAS NOT "N".
      *  MJD 150317 REMOVED IS????? COPYBOOKS.
      *
      * KEC 2016.0502 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         REMOVED USE OF LIBCL/CLMENUW_HLD.CPY AND NOW USING
      *         EXT-WI-CL-SIZE (NEW 78; 200 BYTES).
      * KEC 2016.0510 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         CHANGED USE OF APPNO TO APP-NUMBER FOR MA1-KEY USAGE.
      * BLM 160623 REMOVE WCBC FROM F10 INTERRUPT
      * BAH 2018.0822 LARM30 REMOVED LOAN APPLICATIONS
      *  CS 190415 CHANGE DISPLAY OF WINDOW TO USE SCREEN LINES. WHEN WITHIN
      *            A WINDOW AND PRESSED F10, IT USES THE WINDOW YOU ARE IN 
      *            AS A START POINT AND NOT THE LINE ON THE SCREEN QA0003
      * BAH 2019.0610 REMOVED TEST OF GP-BULLETIN-BOARD NOT SPACES, ALWAYS
      *               DISPLAY BULLETIN BOARD AS A SELECTION
      * BAH 2022.0314 REMOVED EXT-SPANISH-FORMS AND ALL MX FORMS, REMOVED 
      *               SIMENU
      * BAH 2024.0122 ADDED BILNSC-CALLED-FROM 
      * BAH 2024.0416 REMOVED LNQUOT-GIFTNET-FG
      * BAH 2025.0320 SAVE EXT-WI-CL-WORKER TO HLD-WI-CL-WORKER WHEN THE
      *               WINDOW DISPLAYS AND MOVE IT BACK IN THE END-ROUTINE
      *               MIKE WAS ONLY DOING THIS FOR CHECK PRINT AND IF
      *               YOU WERE COLLECTION INQUIRY AND F10 SELECT PAYMENT POST
      *               AND PULL UP AN ACCOUNT IN THERE, F7 EXIT BACK TO WHERE
      *               YOU WERE AND F9 BORROWER, YOU GOT THE INFO FOR THE
      *               PAYMENT ACCOUNT! S35Q-188
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.

      ******************************************************************
      *
      *    W O R K I N G - S T O R A G E - S E C T I O N
      *
      ******************************************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(37) VALUE
           "@(#)CL/WIMENU S35 03/20/25-15:48:28 >".

       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".

       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/GB01GP.CPY".

      ******************************************************************
      *
      *    P R O G R A M   W O R K E R S
      *
      ******************************************************************

       01 WIMENU-WINDOW-HANDLE             PIC X(10).
       01 HOLD-TCLP-MAIN-WINDOW-HANDLE     PIC X(10).
       01 TEMP-WINDOW-HANDLE               PIC X(10).

       01  ROUTE-BUF               PIC XX  VALUE " ".
       01  EXT-EOBUF-MESS              REDEFINES ROUTE-BUF
                                   PIC XX.
       01  SUB                     PIC 9(6)     COMP.

       01  BWLNS3-LINK-BUF.
           03  BWLNS3-SCANYN      PIC X(1) VALUE "N".
           03  BWLNS3-LINK-ACCTNO PIC 9(8) VALUE 0.


      ******************************************************************
      *
      *     F L D S E L - B U F
      *
      ******************************************************************
       01  FLDSEL-FIELD                PIC X(4)        VALUE "010.".
       01  FLDSEL-STAT                 PIC XX.

       01  FLDSEL-LIST.
           03  FILLER                  PIC X(28)       VALUE
           "010 020 030 040 050 060 070 ".
           03  FLDSEL-VARIABLE-LIST    PIC X(32)       VALUE
           "080 090 100 110 120 130 140 150.".

      * IF ONLY 7 FIELDS, WHICH IS THE MINIMUM, IT MOVES SETUP-070 TO
      * FLDSEL-VARIABLE-LIST ABOVE WHICH THEN PUTS THE PERIOD THERE FOR
      * ONLY 7 FIELDS.
      * IF 8 FIELDS, IF MOVES SETUP-080 WHICH THEN ADDS JUST FIELD 080
       01  FLDSEL-LISTS-SETUPS.
           03  FLDSEL-LIST-SETUP-070     PIC X(1)      VALUE
               ".".
           03  FLDSEL-LIST-SETUP-080     PIC X(4)      VALUE
               "080.".
           03  FLDSEL-LIST-SETUP-090     PIC X(8)      VALUE
               "080 090.".
           03  FLDSEL-LIST-SETUP-100     PIC X(12)      VALUE
               "080 090 100.".
           03  FLDSEL-LIST-SETUP-110     PIC X(16)     VALUE
               "080 090 100 110.".
           03  FLDSEL-LIST-SETUP-120     PIC X(20)     VALUE
               "080 090 100 110 120.".
           03  FLDSEL-LIST-SETUP-130     PIC X(24)     VALUE
               "080 090 100 110 120 130.".
           03  FLDSEL-LIST-SETUP-140     PIC X(28)     VALUE
               "080 090 100 110 120 130 140.".
           03  FLDSEL-LIST-SETUP-150     PIC X(32)     VALUE
               "080 090 100 110 120 130 140 150.".

       01  FLDSEL-BUF.
           05  FLDSEL-KEY                PIC X(2).
           05  FLDSEL-NAME               PIC X(26).
           05  FILLER                    PIC X.

       01  FIRST-FIELD                   PIC X(4)      VALUE "010.".
       01  LAST-FIELD                    PIC X(4)      VALUE "150.".

      ******************************************************************
      *
      *     D I S P L A Y - B U F
      *
      ******************************************************************
       01  DISPLAY-LIST.
           03  FILLER              PIC X(32)  VALUE
           "010 020 030 040 050 060 070 080 ".
           03  FILLER              PIC X(44)  VALUE
           "090 100 110 120 130 140 150 PGNO ENDMORE:SF.".
       01  DISPLAY-BUF.
           03  D-LINE              OCCURS 15 TIMES.
               05  D-KEY           PIC X(2).
               05  D-NAME          PIC X(26).
               05  FILLER          PIC X.
           03  D-PAGENO            PIC ZZ9.
           03  D-END-MORE          PIC X(7).

      * ACCTNO IS PASSED IN CALL TO RONPF.C, MUST BE NON COMP.
       01  ACCTNO                  PIC 9(8).
      ******************************************************************
      *
      *     C O P Y - L I B R A R Y - W O R K E R S
      *
      ******************************************************************
      *COPY "LIBLA/APPLICW.CPY".

       77  SCRN-LINES PIC 9(4).
       77  SCRN-SIZE PIC 9(4).

       COPY "LIBWI/PFSWINDOWSW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBWI/LNQUOTW.CPY".
       COPY "LIBLP/LPLOANW.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/FLDSELW.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBWI/WIMENU_DEF.CPY".
       COPY "LIBWI/WIMENU_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PASSWDW.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBWI/BILNSCW.CPY".
       COPY "LIBGB/GETENVW.CPY".
      *-----------------------------------------------------------------
       COPY "LIBCL/CLMENUW_EXT.CPY".
       01  HLD-WI-CL-WORKER        PIC X(EXT-WI-CL-SIZE)   VALUE SPACES.
      *-----------------------------------------------------------------

       COPY "LIBGB/GETFMW.CPY".
       01  WIMENU-SELECTION    PIC XX.

       SCREEN SECTION.
       COPY "LIBWI/WIMENU_SCN.CPY".

      ******************************************************************
      *
      *     C O N T R O L   M O D U L E
      *
      ******************************************************************
       PROCEDURE DIVISION
            USING FORM-PATHNAME WIMENU-SELECTION EXIT-PATHNAME.

      ******************************************************************
      *
      *    M A I N - P R O G R A M
      *
      ******************************************************************
       MAIN-PROGRAM SECTION.

           PERFORM SQL-CONNECT.

      D    STOP MESS.
           MOVE FORM-PATHNAME TO FORM-PATH.

      * THIS CALL TO SETENV IS DONE IN GB/TCLP.C, AND DONE ON F10 INTERRUPT
      * TO PROVIDE A MEANS OF REFRESHING THE ENVIRONMENT.
      * NEED THIS MOVE STATEMENT SO IF AN ERROR OCCURS, YOU DISPLAY THE CORRECT
      * PROGRAM NAME IN SQL-IO-VALIDATION
           MOVE "GB/SETENV" TO FORM-NAM.
           CALL "SETENV" USING FORM-PATH.
           CANCEL "SETENV".
      * RESET THIS BACK TO MENU YOU CALLED IT FROM
           MOVE FORM-PATHNAME TO FORM-PATH.

           ACCEPT GP-REC             FROM ENVIRONMENT "GPENV".

           MOVE EXT-CONAME-USER-LOC TO BR-NO.
           PERFORM OPEN-BR-FILE.
           PERFORM READ-BR-FILE.
           PERFORM CLOSE-BR-FILE.
           IF IO-FG NOT = 0
              MOVE "N" TO BR-WIMENU-CHECK-WRITING.

           MOVE EXT-WI-CL-WORKER TO HLD-WI-CL-WORKER.

           PERFORM SETUP-WINDOW.

       SELECT-FIELD-FROM-DISPLAY.

      *-----------------------------------------------------------
      * STARRED OUT SINCE IT WAS MESSING UP THE DISPLAY BUFFER
      * AND WE HAVE NO EXPLAINATION ???????????????????????
      *
      *    MOVE " F7-EXIT, SELECT OPTION(RTN):" TO MESS.
      *    PERFORM SEND-LEGEND.
      *-----------------------------------------------------------

           PERFORM CALL-FLDSEL.

      * TEST UP ARROW:

           IF FLDSEL-STAT = "1U"
              PERFORM ALARM
              GO TO SELECT-FIELD-FROM-DISPLAY.

      * TEST PAGE UP:

           IF FLDSEL-STAT = "1P"
              PERFORM ALARM
              GO TO SELECT-FIELD-FROM-DISPLAY.

      * TEST PAGE DOWN OR DOWN ARROW:

           IF FLDSEL-STAT = "1N" OR FLDSEL-STAT = "1D"
                 PERFORM ALARM
                 GO TO SELECT-FIELD-FROM-DISPLAY.

      * TEST F7
           IF FLDSEL-STAT = "1>"
              GO TO END-ROUTINE.

           IF NOT (FLDSEL-STAT = "00")
              PERFORM ALARM
              GO TO SELECT-FIELD-FROM-DISPLAY.

      * ADDED THIS BECAUSE ANY PROGRAM THAT HAS SECURITY SET, COMES BACK
      * WITH "3S" IN FORM-OBJ AND I WANT TO CLEAR IT HERE FOR ALL OF THE
      * CALL FORM-PROGX PROGRAMS
      * EXAMPLE: IF YOU PUT LA/LAMENU WITH N FOR ALL PROFILES AND TRY TO
      * RUN IT, FIRST TIME IN IT STOPS YOU, JUST STAYS AT THE F10 MENU,
      * BUT SECOND TIME YOU WILL GET A MISSING PROGRAM /USR/A30/3S/LA/LAMENU
      * CAUSE GETFM PUT 3S IN FORM-OBJ AND WENT TO EXIT-PROG FIRST TIME IN.
      * THEN WE LOOP BACK UP HERE FOR A NEW ENTRY AND I WANT TO RESET IT
      * TO "CO" HERE.

      D    STOP MESS.
           MOVE "CO" TO FORM-OBJ.

           IF FLDSEL-BUF = "LOAN QUOTER"
              PERFORM LOAN-QUOTE
           ELSE
           IF FLDSEL-KEY = "SA"
              PERFORM SALES-QUOTE
           ELSE
           IF FLDSEL-KEY = "PA"
              PERFORM PAYMENT-POST
           ELSE
           IF FLDSEL-KEY = "BO"
              PERFORM BORROWER-CHANGES
           ELSE
           IF FLDSEL-KEY = "GL"
              PERFORM GLOBAL-SCAN
           ELSE
           IF FLDSEL-KEY = "AC"
              PERFORM LOAN-INQUIRY
           ELSE
           IF FLDSEL-KEY = "AP"
              PERFORM APPROVAL-CHANGES
           ELSE
           IF FLDSEL-KEY = "BA"
              PERFORM BALLOON-LOAN-QUOTE
           ELSE
           IF FLDSEL-KEY = "BU"
              PERFORM BULLETIN-BOARD
           ELSE
           IF FLDSEL-KEY = "CH"
              PERFORM CHECK-WRITING
           ELSE
               PERFORM INTER-BR-PAYMENTS.

           GO TO SELECT-FIELD-FROM-DISPLAY.

      ******************************************************************
      *    P A Y M E N T - P O S T
      ******************************************************************
       PAYMENT-POST SECTION.
      * THIS NEEDS TO BE THE PROGRAM TO TEST FOR SECURITY
           MOVE "LP/LONPF" TO FORM-NAM.
           MOVE 0 TO ACCTNO.
           CALL "RONPF" USING FORM-PATH ACCTNO " " PROG-BUF
                                                 EXIT-PATHNAME.
           CANCEL "RONPF".

      ******************************************************************
      *    S A L E S - Q U O T E R
      ******************************************************************
       SALES-QUOTE SECTION.
           MOVE "WI/SFQUOT" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH EXIT-PATHNAME.
           CANCEL FORM-PROGX.

      ******************************************************************
      *    B A L L O O N   L O A N   Q U O T E R
      ******************************************************************
       BALLOON-LOAN-QUOTE SECTION.
           MOVE "WI/BLQUOT" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH EXIT-PATHNAME.
           CANCEL FORM-PROGX.

      ************************
       CHECK-WRITING SECTION.
      ************************
           CLOSE WINDOW WIMENU-WINDOW-HANDLE.
           PERFORM TEMP-WINDOW-OPEN.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
      * THIS NEEDS TO BE PROGRAM TO CHECK FOR SECURITY
           MOVE "LP/CHCKI0" TO FORM-NAM.
           CALL "RONPI" USING FORM-PATH EXIT-PATHNAME.
           CANCEL "RONPI".
           PERFORM CLOSE-WINDOW UNTIL WINDOWS-OPEN = 0.
           PERFORM TEMP-WINDOW-CLOSE.
           PERFORM SETUP-WINDOW.

      ******************************************************************
      *    B U L L E T I N  B O A R D
      ******************************************************************
       BULLETIN-BOARD SECTION.
           MOVE "WI/BBINQ" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH EXIT-PATHNAME.
           CANCEL FORM-PROGX.

      ******************************************************************
      *    B O R R O W E R - C H A N G E S
      ******************************************************************
       BORROWER-CHANGES SECTION.
           MOVE "WI/MODBOR" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH EXIT-PATHNAME.
           CANCEL FORM-PROGX.

      ******************************************************************
      *    G L O B A L - S C A N
      ******************************************************************
       GLOBAL-SCAN SECTION.
           MOVE "G"                 TO BILNSC-ERROR.
           MOVE "WIMENU"            TO BILNSC-CALLED-FROM.
           MOVE 0                   TO BILNSC-SSNO.
           MOVE EXT-CONAME-SYSDATE  TO BILNSC-SYSDATE.
           MOVE EXT-CONAME-USER-LOC TO BILNSC-BRANCH.

           MOVE "WI/BILNSC" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH BILNSC-BUF EXIT-PATHNAME.
           CANCEL FORM-PROGX.

      ******************************************************************
      *    L O A N - Q U O T E
      ******************************************************************
       LOAN-QUOTE SECTION.
           MOVE "WI/LNQUOT" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH LNQUOT-BUF EXIT-PATHNAME.
           CANCEL FORM-PROGX.

      ******************************************************************
      *    L O A N - I N Q U I R Y
      ******************************************************************
       LOAN-INQUIRY SECTION.
           IF EXT-CONAME-NEW-COLL-INQUIRY = "Y"
                AND WIMENU-SELECTION = "C1"
              MOVE "SP/XPINQ" TO FORM-NAM
              CALL "XPINQ" USING FORM-PATH EXIT-PATHNAME
                                   BWLNS3-LINK-BUF
              CANCEL "XPINQ"
           ELSE
              MOVE "SP/CPINQ0" TO FORM-NAM
              CALL "CPINQ0" USING FORM-PATH " " "Y"
                                   PROG-BUF EXIT-PATHNAME
              CANCEL "CPINQ0".

      ******************************************************************
      *    A P P R O V A L - C H A N G E S
      ******************************************************************
       APPROVAL-CHANGES SECTION.
           MOVE "WI/GLIMIT" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH EXIT-PATHNAME.
           CANCEL FORM-PROGX.

      ******************************************************************
      *    I N T E R - B R - P A Y M E N T S
      ******************************************************************
       INTER-BR-PAYMENTS SECTION.
           MOVE 0 TO PASSWDW-SEQ.
           PERFORM VERIFY-PASSWORD.
           IF PASSWDW-STAT = "OK"
              MOVE "LP/INBRTR" TO FORM-NAM
              MOVE 0 TO ACCTNO
              CALL "INBRTR" USING FORM-PATH ACCTNO " " PROG-BUF
                                                   EXIT-PATHNAME
              CANCEL "INBRTR"
           ELSE
              MOVE "NOT ALLOWED WITHOUT VALID PASSWORD" TO MESS
              PERFORM SEND-MESS.

      ******************************************************************
      *
      *    S E T U P - W I N D O W
      *
      ******************************************************************
       SETUP-WINDOW SECTION.

           MOVE "WIMENU" TO VDU-FORM-NAME.
      * IF THESE 2 LINES ARE NOT HERE, CHKSEC DOESNT CHECK FOR WIMENU
      * IT GETS THE LAST PROGRAM CALLED.
           MOVE "CL/WIMENU" TO FORM-NAM.
           MOVE FORM-PATH TO FORM-PATHNAME.

           DISPLAY FLOATING GRAPHICAL WINDOW
                   AT SCREEN LINE 04 * VDU-WIN-L-OFFSET
                   AT SCREEN COL 26 * VDU-WIN-C-OFFSET
                   SIZE 30 + VDU-WIN-W-MOD
                   LINES 19 + VDU-WIN-H-MOD
                   CELL SIZE IS LABEL FONT
                   COLOR IS VDU-WCOLOR-FWIN
                   CONTROL VALUE IS VDU-WCTRL-FWIN
                   BLANK SCREEN
                   HANDLE  IS WIMENU-WINDOW-HANDLE.
      
       COPY "LIBGB/CHKSEC.CPY".
      
           PERFORM FORM-RESET.

           PERFORM SETUP-DISPLAY-BUFFER.

           MOVE "* END *" TO D-END-MORE.
           MOVE 1         TO D-PAGENO.

       SETUP-WINDOW-EXIT.
           EXIT.

      ******************************************************************
       SETUP-DISPLAY-BUFFER SECTION.
      ******************************************************************

           MOVE SPACES TO DISPLAY-BUF.

           IF ( WIMENU-SELECTION = "C" )
              PERFORM SETUP-DISPLAY-WORLD-1
           ELSE
              IF ( WIMENU-SELECTION = "C0" )
                 PERFORM SETUP-DISPLAY-WORLD-2
              ELSE
                 IF ( WIMENU-SELECTION = "Y" OR "C1" OR "2")
                    PERFORM SETUP-DISPLAY-WORLD-3.

       EXIT-SETUP-DISPLAY-BUFFER.
           EXIT.


      *********************************************************************
      * "C" = NO LOAN PROCESSING INTERRUPTS, PAYMENT POSTING, CHECK WRITING
      *********************************************************************
       SETUP-DISPLAY-WORLD-1 SECTION.

           MOVE "AC"                 TO D-KEY (1).
           MOVE "COUNT INQUIRY"      TO D-NAME(1).
           MOVE "BO"                 TO D-KEY (2).
           MOVE "RROWER CHANGES"     TO D-NAME(2).
           MOVE "GL"                 TO D-KEY (3).
           MOVE "OBAL LOAN SCAN"     TO D-NAME(3).
           MOVE "LO"                 TO D-KEY (4).
           MOVE "AN QUOTER"          TO D-NAME(4).
           MOVE "SA"                 TO D-KEY (5).
           MOVE "LES FINANCE QUOTER" TO D-NAME(5).
           MOVE "AP"                 TO D-KEY (6).
           MOVE "PROVAL LIMIT"       TO D-NAME(6).
           MOVE 6                        TO SUB.
           IF EXT-CONAME-BALLOON-LOANS-FG = "Y"
              ADD 1                      TO SUB
              MOVE "BA"                  TO D-KEY (SUB)
              MOVE "LLOON LOAN QUOTER"   TO D-NAME(SUB).

           ADD 1                     TO SUB.
           MOVE "BU"                 TO D-KEY (SUB).
           MOVE "LLETIN BOARD     "  TO D-NAME(SUB).

           PERFORM SET-FLDSEL-VARIABLE-LIST.

      ***************************************************************
      * "C0" = NO ACCOUNT INQUIRY,
      ***************************************************************
       SETUP-DISPLAY-WORLD-2 SECTION.

           MOVE "BO"                  TO D-KEY (1).
           MOVE "RROWER CHANGES"      TO D-NAME(1).
           MOVE "GL"                  TO D-KEY (2).
           MOVE "OBAL LOAN SCAN"      TO D-NAME(2).
           MOVE "LO"                  TO D-KEY (3).
           MOVE "AN QUOTER"           TO D-NAME(3).
           MOVE "PA"                  TO D-KEY (4).
           MOVE "YMENT POSTING"       TO D-NAME(4).
           MOVE "SA"                  TO D-KEY (5).
           MOVE "LES FINANCE QUOTER"  TO D-NAME(5).
           MOVE "IN"                  TO D-KEY (6).
           MOVE "TER BRANCH PAYMENTS" TO D-NAME(6).
           MOVE "AP"                  TO D-KEY (7).
           MOVE "PROVAL LIMIT"        TO D-NAME(7).
           MOVE 7                      TO SUB.
           IF EXT-CONAME-BALLOON-LOANS-FG = "Y"
              ADD 1                      TO SUB
              MOVE "BA"                  TO D-KEY (SUB)
              MOVE "LLOON LOAN QUOTER"   TO D-NAME(SUB).

           ADD 1                      TO SUB.
           MOVE "BU"                  TO D-KEY (SUB).
           MOVE "LLETIN BOARD     "   TO D-NAME(SUB).

           PERFORM SET-FLDSEL-VARIABLE-LIST.


      ****************************************************************
      * "2" NO CHECK WRITING "C1" HAS COLLECT INQ  "Y" HAS OLD INQUIRY
      ****************************************************************
       SETUP-DISPLAY-WORLD-3 SECTION.

           MOVE "AC" TO D-KEY(1).

           IF ( WIMENU-SELECTION = "Y" )
              MOVE "COUNT INQUIRY" TO D-NAME(1)
           ELSE
              MOVE "COUNT INQUIRY (COLLECTOR)" TO D-NAME(1).

           MOVE "BO"                  TO D-KEY (2).
           MOVE "RROWER CHANGES"      TO D-NAME(2).
           MOVE "GL"                  TO D-KEY (3).
           MOVE "OBAL LOAN SCAN"      TO D-NAME(3).
           MOVE "LO"                  TO D-KEY (4).
           MOVE "AN QUOTER"           TO D-NAME(4).
           MOVE "PA"                  TO D-KEY (5).
           MOVE "YMENT POSTING"       TO D-NAME(5).
           MOVE "SA"                  TO D-KEY (6).
           MOVE "LES FINANCE QUOTER"  TO D-NAME(6).
           MOVE "IN"                  TO D-KEY (7).
           MOVE "TER BRANCH PAYMENTS" TO D-NAME(7).
           MOVE "AP"                  TO D-KEY (8).
           MOVE "PROVAL LIMIT"        TO D-NAME(8).
           MOVE 8                     TO SUB.

           IF EXT-CONAME-BALLOON-LOANS-FG = "Y"
              ADD 1                      TO SUB
              MOVE "BA"                  TO D-KEY (SUB)
              MOVE "LLOON LOAN QUOTER"   TO D-NAME(SUB).

           ADD 1                         TO SUB
           MOVE "BU"                     TO D-KEY (SUB)
           MOVE "LLETIN BOARD     "      TO D-NAME(SUB).

           PERFORM SET-FLDSEL-VARIABLE-LIST.

      *******************************************
       SET-FLDSEL-VARIABLE-LIST SECTION.
      *******************************************
           EVALUATE SUB
              WHEN  7 MOVE FLDSEL-LIST-SETUP-070 TO FLDSEL-VARIABLE-LIST
                      MOVE "070." TO LAST-FIELD
              WHEN  8 MOVE FLDSEL-LIST-SETUP-080 TO FLDSEL-VARIABLE-LIST
                      MOVE "080." TO LAST-FIELD
              WHEN  9 MOVE FLDSEL-LIST-SETUP-090 TO FLDSEL-VARIABLE-LIST
                      MOVE "090." TO LAST-FIELD
              WHEN 10 MOVE FLDSEL-LIST-SETUP-100 TO FLDSEL-VARIABLE-LIST
                      MOVE "100." TO LAST-FIELD
              WHEN 11 MOVE FLDSEL-LIST-SETUP-110 TO FLDSEL-VARIABLE-LIST
                      MOVE "110." TO LAST-FIELD
              WHEN 12 MOVE FLDSEL-LIST-SETUP-120 TO FLDSEL-VARIABLE-LIST
                      MOVE "120." TO LAST-FIELD
              WHEN 13 MOVE FLDSEL-LIST-SETUP-130 TO FLDSEL-VARIABLE-LIST
                      MOVE "130." TO LAST-FIELD
              WHEN 14 MOVE FLDSEL-LIST-SETUP-140 TO FLDSEL-VARIABLE-LIST
                      MOVE "140." TO LAST-FIELD
              WHEN 15 MOVE FLDSEL-LIST-SETUP-150 TO FLDSEL-VARIABLE-LIST
                      MOVE "150." TO LAST-FIELD
           END-EVALUATE.

      ******************************************************************
       VERIFY-PASSWORD SECTION.
      ******************************************************************

           MOVE SPACES                       TO PASSWDW-STAT.
           MOVE "WIMENU"                     TO PASSWDW-PGM.
           MOVE "PLEASE ENTER THE PASSWORD:" TO PASSWDW-PROMPT1.
           MOVE SPACES                       TO PASSWDW-PROMPT2.
           MOVE "CL/PASSWD"                  TO FORM-NAM.

           CALL "PASSWD" USING FORM-PATH PASSWDW-BUF.
           CANCEL "PASSWD".

           IF ( PASSWDW-STAT NOT = "OK" )
              MOVE "NOT ALLOWED WITHOUT THE CORRECT PASSWORD!!" TO MESS
              PERFORM SEND-MESS.

       EXIT-VERIFY-PASSWORD.
           EXIT.

      ******************************************************************
      *
      *    C O P Y - L I B R A R Y - S E C T I O N S
      *
      ******************************************************************
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/FLDSEL.CPY".
       COPY "LIBGB/DATER.CPY".
       COPY "LIBWI/PFSWINDOWS.CPY".

      **************************************************************************
       FORM-RESET SECTION.
      **************************************************************************
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY WIMENU-SCREEN.
           DISPLAY WIMENU-SCREEN UPON WIMENU-WINDOW-HANDLE.
           MOVE WIMENU-FORM-FIELDS TO WR-BUF.
           MOVE WIMENU-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
       FORM-RESET-EXIT.
           EXIT.

      **************************************************************************
       SETUP-LINK-INFO SECTION.
      **************************************************************************
           MOVE WIMENU-HELP--FILE TO HELP-LINK-FILE.
           MOVE WIMENU-HELP--DIR  TO HELP-LINK-DIR.

      **************************************************************************
       VDU-CONVERT-FIELD SECTION.
      **************************************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBWI/WIMENU_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.


      *****************************************************************
      * OPEN UP A TEMPORARY WINDOW OVER TOP OF THE MAIN TCLP WINDOW AND
      * PASS THE HANDLE FORWARD IN THE TCLP-MAIN-WINDOW HANDLE
      * AFTER BACKING UP THE TCLP-MAIN-WINDOW HANDLE.)
      *
      * IF WE CALL A MENU FROM THE F10 AND THE MENU DISPLAY ON THE
      * TCLP-MAIN-WINDOW-HANDLE THEN WE ARE DISPLAYING OVER THE EXISTING
      * SCREEN AND IT LOOKS JUMBLED.   AT THE TIME OF THE CALL I DO
      * NOT KNOW WHAT SCREEN IS DISPLAYED AND PERFORMING A DESTROY
      * ALL CONTROLS MAY GET RID OF WORK THE USER HAS NOT COMPLETED.
      *
      * TO GET AROUND THIS I AM SAVING THE TCLP-MAIN-WINDOW HANDLE IN
      * A WORKER, OPENING A NEW WINDOW OVER TOP OF THE TCLP-MAIN-WINDOW
      * HANDLE AND THEM MOVING THE NEW WINDOWS HANDLE TO TCLP-MAIN-WINDOW
      * HANDLE.   SUBSEQUENT PROGRAMS THAT DISPLAY TO TCLP-MAIN WINDOW
      * HANDLE WILL NOT AUTOMATICALLY DISPLAY TO THIS NEW TEMP WINDOW.
      * UPON RETURNING FROM THE CALLED PROGRAM, THE WINDOW MUST BE
      * CLOSED AND THE TCLP-MAIN-WINDOW HANDLE RESTORED SO THAT
      * EXISTING PROGRAMA CAN CONTINUE TO FUNCTION PROPERLY.
      *****************************************************************
       TEMP-WINDOW-OPEN SECTION.
           MOVE TCLP-MAIN-WINDOW-HANDLE TO HOLD-TCLP-MAIN-WINDOW-HANDLE.

      * BACK UP WORKER AS VALUES MAY CHANGE IN CALLED PROGRAM AND WE DO NOT
      * WANT IT TO AFFECT PROGRAM YOU JUST INTERRUPED OUT OF.
           MOVE EXT-WI-CL-WORKER TO HLD-WI-CL-WORKER.

      * FORCE TO 80X24.  IF NOT VEERYANT WILL FILL ENTORE SCREEN WITH WINDOW.
      *  IF CHANGING HERE CHANGE CL/PGMENU, CL/SIMENU, GB/TCLP AS WELL.
            MOVE 80  TO SCRN-SIZE.
            MOVE 24 TO SCRN-LINES.

           DISPLAY FLOATING GRAPHICAL WINDOW
                   SIZE SCRN-SIZE + VDU-WIN-W-MOD
                   LINES SCRN-LINES + VDU-WIN-H-MOD
                   CELL SIZE IS LABEL FONT
                   COLOR IS VDU-WCOLOR-TCLP
                   CONTROL VALUE IS VDU-WCTRL-TCLP
                   BLANK SCREEN
                   HANDLE IS TEMP-WINDOW-HANDLE.

           MOVE TEMP-WINDOW-HANDLE TO
                TCLP-MAIN-WINDOW-HANDLE.
  
       TEMP-WINDOW-CLOSE SECTION.
           DESTROY TEMP-WINDOW-HANDLE
           MOVE HOLD-TCLP-MAIN-WINDOW-HANDLE TO TCLP-MAIN-WINDOW-HANDLE.
      * RESTORE  UP WORKER AS VALUES MAY CHANGE IN CALLED PROGRAM AND WE DO NOT
      * WANT IT TO AFFECT PROGRAM YOU JUST INTERRUPED OUT OF.
           MOVE HLD-WI-CL-WORKER TO EXT-WI-CL-WORKER.
    
      ******************************************************************
       MISC SECTION.
      ******************************************************************
       COPY "LIBGB/GETENV.CPY".
       COPY "LIBGB/SYSTEM.CPY".
       COPY "LIBGB/ACCESS.CPY".
       SEND-LEGEND.
           MOVE MESS      TO WR-BUF.
           MOVE "WSEL:R." TO WR-LIST.
           PERFORM CALL-WRFORM.

      ******************************************************************
       IO-ROUTINES SECTION.
      ******************************************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-BR-FILE.
       COPY "LIBGB/GBBRGS_SQL.CPY".
           COPY "LIBGB/GBBRRN.CPY".

      ******************************************************************
       IO-ERROR SECTION.
      ******************************************************************

           MOVE "WINDOW ERROR " TO MESS.
           PERFORM SEND-MESS.

       END-ROUTINE.
           MOVE HLD-WI-CL-WORKER TO EXT-WI-CL-WORKER.

       EXIT-PROG.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY WIMENU-SCREEN.
           DESTROY WIMENU-WINDOW-HANDLE.

       STOP-RUN.
           EXIT PROGRAM.
