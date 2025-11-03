       IDENTIFICATION DIVISION.
       PROGRAM-ID.      MODBOR.
       DATE-WRITTEN.    080492.
      *****************************************************
      *                 (WI)
      *
      *  DESCRIPTION:   CHANGE BORROWER RECORD.
      *
      * CALLED FROM:
      *               WI/WIMENU -  F10 INTERRUPT
      *
      * REV:
      *  101695 BAH DONT FORCE BW-SP-BDATE
      *  MJD 030196 ADDED LOGIC TO BLOCK ENTRY OF 999999999 AS SS#
      *  JFF 093096 ADDED CODE-SCAN FOR NEW CD-TYPE, CDTYPE-BW
      *                #13-BORROWER HOUSING CODE
      *  MJD 120996 FORCE ENTRY OF CITY/STATE IF ADDRESS 1/2 IS ENTERED.
      *             REMOVED NUMERIC TEST OF ZIP.
      *  BLV 022097 ADD BW-CREDIT-SCORE, FIELD 16-1
      *  KEC 041697 CHANGED ACCESS TO BW-REPCODE FROM NUMERIC TO ALPHA.
      *  MJD 050997 ADDED GP-BW-CREDIT-SCORE TO FORCE ENTRY OF BW-CREDIT-SCORE.
      *             REQ#E155.
      *  JTG 082197 ADDED BW-OTHPHONE LOGIC WORLD A90
      *  GLF 090997 REPLACED 'IO' ON GP-FILE WITH GET 'GPENV'
      *  MJD 112497 EXPANDED BW-REC TO 480 BYTES
      *  MJD 021898 MOVED BWCRE2 TO BWCRE3. ADDED NEW BWCRE2 - BORROWER
      *             PHONE NUMBERS ADDED 78 LEVEL FOR BW-SIZE.
      *  GLF 030298 REMOVED BW-OTHPHONE AND ADJUSTED FIELDS ACCORDINGLY
      *  CS  111398 ADDED BW-CREDIT-LIMIT
      *  MJD 990601 ADDED PASSWORD TO CHANGE OF CREDIT SCORE
      *             (REQ#865 - REGIONAL-ACCEPT.)
      *  GLF 990625 ADDED PASSWORD TO CHANGE REFERENCE DATE (PR#207 WORLD)
      *  GLF 991709 CHANGE SSNO ENTRY TO USE NEW VDUCLASS "X" EDIT
      *  BLV 991203 DON'T ALLOW ALL 9'S FOR SPOUSE'S SOC - WILL PUT
      *             USER IN LOOP &  WILL HAVE TO BE KILLED
      *  BAH 000417 ADD PASSWORD FOR CHANGING CONFIDENTIAL CODE, PR# 79 TFC
      *                 WI MODBOR OPT=2
      *  MJD 000502 ADDED LOGIC FOR CONTACT PHONE NUMBERS IN BM-REC(TFC PR#37)
      *  BAH 001004 FORCE BW-REPCODE ENTRY TO "YES" OR "NO" FOR TAX PRE
      *             REGISTRATION, WORLD PR# 240
      *  BAH 010228 DO NOT WRITE A BORROWER MEMO FOR ANY CHANGES IF
      *             GP-I-AM-MODERN, MODERN PR# 1437
      *  BAH 010627 ADDED BW-PRIVACY-OPT-OUT 37. OPT OUT : FOR TFC , PR# 90
      *             AND PASSWORD TURNING IT OFF.
      *  BAH 010711 PASSWORD SMSA (BW-STRTGCD) FOR WORLD, PR# 269
      *  CS  010823 DONT ALLOW ENTRY OF BW-PRIVACY-OPT-OUT IF
      *             GP-BW-PRIVACY-OPT-OUT NOT = "Y" PR#1571 STLOUIS
      *  CS  020220 IF BR-FORCE-CITY-COUNTY = "Y  MUST EXAMINE CODE FILE FOR
      *             "CT" & "CY" ENTRIES FOR STATE. IF ANY ENTRIES EXIST FOR THE
      *             STATE, ANY CITY OR COUNTY ENTRY MUST BE VALIDATED
      *             [WORLD PR#288]
      *  CS  020305 CORRECTED SETUP OF CD-BW-KEY.
      *  CS  020312 REMOVED EXTRA PERIOD IN SET-CY-CT SECTION
      *  BAH 020423 BUG FROM 020220 CHANGE, PHONE WAS REDISPLAYING IN THE
      *             EMP POSITION(FIELD 29) WHICH USED TO BE PHONE
      *  BLV 020612 FIXED NEW RECORD ROUTINE, WAS FORCING "NO" TO SLSMN INSTEAD
      *             OF REP CODE IF I-AM-WORLD
      *  CS  020627 FIXED ENTRY IF SPOUSE-FG = "Y", WAS ALLOWING ENTRY IN SPOUSE      *             AGE BUT NOT AT OTHER PAY
      *  BLV 020628 IF I-AM-WORLD, DON'T STOP AT OPT OUT FLAG (FIELD 38)
      *             ON NEW RECORD
      *  BAH 020808 ADDED PASSWORD ON BW-LNAME, COLLECTORS ARE CHANGING
      *             LAST NAME TO GET THEM OUT OF THEIR QUEUE !! TFC PR# 94
      *  BAH 021007 IF PHONE CONTACTS ARE SPACED OUT (DELETED), THEN DELETE
      *             THE BORROWER MEMO RECORD !! TFC
      *  CS  012103 IF SPACE ENTERED AS BW-CITY-TAX-CD AND BR-FORCE-VALID-CITY
      *             COUNTY = "Y" GIVE A WARNING MESSAGE [WORLD PR#288]
      * BAH 030205 ADDED BW-CALL-ON-SUNDAY FIELD 39.  REGACC PR# 15
      * BAH 030410 ADDED BW-BAD-ADR-FG AND BW-BAD-PHONE-FG TRIAD REGACC PR# 26
      * BAH 030430 ADDED SET OF BW-ADR-PHONE-CHG-DATE FOR TRIAD REGACC PR# 26
      * BAH 030520 ALLOW PAGE 4, BWCRE4, REGACC PR# 27 AND
      *            DISPLAY "ALT: Y" IF BX ALTERNATE ADDRESS EXIST
      * BAH 030728 ADDED BW-BAD-EMPPHONE-FG TRIAD REGACC PR# 36
      * BAH 031114 ADDED GP-USE-BW-BAD-FLAGS FOR THE ENTRY OF THE ABOVE REGACC
      *             FIELDS...SUPPORT PR# 2155
      * BAH 040218 AUTOMATICALLY SET BW-BAD-PHONE-FG FOR SPECIFIC
      *            PHONE NUMBERS, REGACC PR# 48
      * BAH 040301 ADDED UPDATE-BI-FILE, BI-STAT = "F" TO UPDATE
      *            ANY CHANGE TO LNAME AND FNAME, PCCREDIT PL# 389
      * BAH 040409 CHANGE PROMPT 24 SOLICIT TO 24 CONTACT AND PROMPT 39 FROM
      *            CALL SUNDAY TO 39 OK SOLICIT FOR LENDMARK PR# 2271
      *            ALSO ADDED PASSWORDS FOR FIELD 24 AND 38
      * BAH 050812 WAS NOT INITIALIZING HOLD-PHONE-CONTACTS ON A NEW RECORD,
      *            SO PAGE 2 WOULD FILL WITH 202020 AND ALSO PULL UP THE
      *            PREVIOUS INFO THAT YOU MAY HAVE USED ON SOMEONE ELSE !
      *            COLTN PL# 509
      * BAH 060501 WAS NOT INITIALIZING ORG-PHONE-CONTACTS ON A NEW RECORD,
      *            SO CREATING CHANGE MEMOS WITH ALL 0'S IN NEW PHONE NUMBER
      *  MJD 061116 ADDED NEW BWCRE5 - BORROWER
      *             "KNOW YOUR CUSTOMER" INFO REGACC                PR#2856.
      * BAH 061117 FORGOT TO DELETE BX TRAILER RECORD, LENDMARK PR# 2044
      * MJD 070129 ADDED PW PROTECTION FOR REFDATE (NEW RECORD MODIFY)
      *            AND SKIPPED FIELD ON NEW RECORD CREATE IF REGENCY.
      *                                                    REGENCY PR#2856
      * BLF 080111 ADD CHECKS ON BORROWER NAME FIELDS, IF
      *            GP-EDIT-BORROWER-NAME-FG = Y, ONLY ALLOW A-Z, 0-9,
      *            SPACE, DASH OR APERSAND,  WORLD PR # 564
      * BLM 080321 ALLOW APOSTROPHE IN NAME IF GP-EDIT-BORROWER-NAME-FG = Y,
      *            REVISION TO WORLD PR# 564
      * BAH 080401 TEST THAT AGE MUST BE GREATER THAN 18 YRS AND BIRTHDATE
      *            CAN NOT BE TODAYS DATE, WORLD PR# 577
      * BAH 080702 ADDED BW-CELL-PHONE FIELD 8. UNITED PR# 3205
      * BAH 090107 WHEN I ADDED BW-CELL-PHONE, I DIDNT FIX THE SPOUSE POP UP
      *            TO HAVE NEW FIELD NAMES, JUST A COSMETIC FIX...BH
      * BAH 090112 DONT ALLOW SAME SSNO FOR BORROWER AND SPOUSE
      *            DONT ALLOW SPOUSE SSNO IF NO SPOUSE NAME, WORLD (EAGLE PASS)
      * BAH 091005 PASSWORD CLEARING ADR,CITY,STATE,ZIP FOR REGACC PR# 256
      * BAH 100106 PASSWORD CHANGING LNAME,FNAME,SECOND LNAME,RFC WORLD
      *               MEXICO PR# 667
      * BAH 100216 WAS NOT GOING TO ZIP IF YOU SPACES OUT THE STATE, WORLD
      * BAH 100608 ADDED BW-BAD-CELL-FG FIELD 081, REGACC PR# 266
      *  CS 100916 IF WORLD, IF NEW REC DON'T ALLOW ENTRY TO 'REPCODE' OR
      *            CREDIT SCORE, IF EXISTS, CHECK FOR PASSWORD, NEW SEQ 11
      *                                                    WRLD #707
      *  CS 101004 NEW RECORD SETUP DO NOT MOVE 'NO' TO REPCODE WLRD#707
      * BLM 110216 ADDED F4 OPTION TO SCAN & VIEW CUSTOMER INFO, ONLY
      *            IF I-AM-WORLD.  CALLS WI/CISCAN WHICH CALLS WI/CIINQ.
      *            WORLD PR# 739
      *  CS 110510 ADD TEST IF GP-I-AM-REGENCY & BR-STATE = "KY" FORCE "KY"
      *            TO CD-CY-STATE & CD-CT-STATE. REGENCY WANTS ALL BORROWERS
      *            IN KY BRANCH TO BE CHARGED INSURANCE TAX REGARDLESS OF
      *            THE BORROWER STATE REGENCY PR#3784
      * BAH 110801 ADDED MXCREA FOR SPANISH POP UP WINDOW, WORLD PR# 754
      * BAH 120417 ADDED SET OF FORM-SPANISH AND MOVED GETFM FOR WORLD PR# 800
      * BAH 120828 PASSWORD CHANGING FIRST NAME FNAME, REGMGM PR# 4100
      * BAH 120830 ADDED SET OF BW-DEALTRACK-CHG-DATE FOR REGACC PR# 319
      * BAH 120927 ADDED BW-WORKHRS-NUM FOR GP-I-AM-MULL, MULLEN PR# 4092
      * BAH 130819 CHANGE 38 OPT OUT FOR WORLD TO TXT OPTION, WORLD PR# 919
      * BAH 140225 CHANGE VALUES ALLOWED FOR WORLD TXT OPTION, WORLD PR# 970
      * BAH 140512 REMOVED FORM-SPANISH ADDED EXT-SPANISH-FORMS
      * MJD 140606 ADDED EXIT-PATHNAME
      * BLM 141001 ALLOW VALUE OF "D" (DECLINED) FOR WORLD TXT OPTION 
      *            (BW-PRIVACY-OPT-OUT), WORLD PR# 1023
      * BAH 150728 ADDED PASSWORD FOR FIELDS 34-37 AND 21, SOAUTO PR# 4810O
      * BAH 150903 FORCE BW-PRIVACY-OPT-OUT TO "N" IF CELL PHONE CHANGES
      *            AND WRITE A MEMO, WORLD PR# 1101
      * BAH 151030 REDISPLAY BW-REPCODE IF BWCRE2 CHANGED IT, HONOR PR#4898
      * BAH 151216 ALLOW "C" IN BAD PHONE FLAGS, SOAUTO PR#4912
      * KEC 2016.0328 CHANGED CDSCAN-WINDOW-BUF TO HAVE MATCHING CD1-KEY
      *               LAYOUTS (FOR EACH CODE TYPE; CD-TYPE).
      *               CDSCAN-CD1-KEY IS THE NEW (PRIMARY) WITH REDEFINES OF
      *               CDSCAN-??-KEY (NEW).
      * KEC 2016.0328 CHANGED CYSCAN WORKERS TO MATCH CD1-KEY FOR CY LAYOUT.
      *               CHANGED CTSCAN WORKERS TO MATCH CD1-KEY FOR CT LAYOUT.
      * BAH 160422 ADDED BI-LN-SMALLOAN FOR DUAL TEST, REGENCY PR#5030
      * BAH 160510 CHANGED PASSWORD 13 FOR JUST CITY, SOAUTO PR#5066
      * BAH 160523 ADDED PASSWORD 14 TO CHANGE BIRTHDATE, SOAUTO PR#5080
      * BAH 160715 DEFAULT BW-REPCODE TO "P" FOR HONOR, IF EMAIL IS SPACES
      *            REPCODE = "P", IF NOT SPACES, IT IS "E". PR#5130
      * BAH 160822 MOVED BM-PHONE-DESC, BM-PHONE-NO AND BM-PHONE-EXT TO BW
      * KEC 2016.0928 ADDED WARNING MESSAGE FOR CHANGE OF %%-BRNO.
      * KEC 2016.0928 ADDED WARNING MESSAGE FOR CHANGE OF BW-BRANCH.
      * BAH 161024 DEFAULT SPOUSE RECORD WHEN IT AUTOMATICALLY FILLS IN ON THE
      *            FLIP TO "N", HONOR PR#5183
      * BAH 161020 ADDED BW-BDATE-YYYYMMDD  AND BW-SP-BDATE-YYYYMMDD
      * MJD 170301 ADDED BLANK ZERO TO DISPLAY BUF FOR F09 AND F10
      * BAH 170414 ACTIVATE 8 DIGIT DATES, PD0006
      * BAH 180207 DO NOT VALIDATE SPOUSE BDATE FOR MEXICO, THEY USE IT FOR
      *            LOAN APP DATE, WORLD PR#1304
      * BAH 180426 ADDED PASSWORD 15 FOR MEXICO TO NOT ALLOW CHANGES TO
      *            AN EXISTING CENSUS AND ORGSLSMN WHICH IS WHERE THEY STORE
      *            EMPLOYEE AND PROMOTER WHO GAVE THE LOAN #1316
      * BAH 180316 ADDED BI-LN-PLDATE TO BICREA CALL LIST, WORLD #1308
      * BAH 180925 GLOBAL FILES
      * BAH 190128 SPLIT CDFILE
      * BAH 190307 REMOVED BW-WORKHRS-NUM REDEFINES, WAS ONLY USED FOR MULLEN
      * BAH 190425 REMOVED MXCREA
      * BAH 20190709 REMOVED GP-LR-TRAILER-TRIAD, PT0151 
      * BAH 02.10.2020 ONLY ALLOW VALID CODES FOR ORIGIN #1421
      * BAH 2021.0914 ASK PASSWORD TO CHANGE BIRTHDATE ONLY IF NOT 0, ALLOW
      *               THEM TO CHANGE IF 0 WITH NO PASSWORD. FORCE A VALID
      *               BIRTHDATE, #1513
      * BLM 210928 FIX CAST ERROR IF ALPHA ENTERED IN SSNO SCAN
      * BAH 2022.0314 REMOVED EXT-SPANISH-FORMS AND ALL MX FORMS
      * BAH 2023.0321 VERIFY-ZIPCODE #1596
      * BAH 2024.0418 REMOVED MODBORW
      * BAH 2024.1204 ADDED BW-CEASE-DESIST-DATE & BW-CEASE-DESIST-TYPE #1686
      *                                                         S35Q-145
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
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)WI/MODBOR.CBL S35 02/22/24-12:52:41 >".
      *    *      PROGRAM WORKERS
      *    **********************************
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBLP/LP01ST.CPY".
       COPY "LIBLP/LP01ST_SQL.CPY".
       COPY "LIBLP/LPWSST.CPY".
       COPY "LIBLP/LP01BW.CPY".
       COPY "LIBLP/LP01BW_SQL.CPY".
       COPY "LIBLP/LPWSBW.CPY".
       COPY "LIBLP/LP01BX.CPY".
       COPY "LIBLP/LP01BX_SQL.CPY".
       COPY "LIBLP/LPWSBX.CPY".
       COPY "LIBLP/LP01CD.CPY".
       COPY "LIBLP/LP01CD_SQL.CPY".
       COPY "LIBLP/LPWSCD.CPY".

       01  MODBOR-LINES              PIC 99  VALUE 22.
       01  MODBOR-COLUMNS            PIC 99  VALUE 78.
       01  MODBOR-BEGIN-Y            PIC 99  VALUE 2.
       01  MODBOR-BEGIN-X            PIC 99  VALUE 2.

      *=================================================================
      * ORG-BW-REC FROM 'REPLACING LEADING' ON BW-REC - ORIGINAL-BW-REC
      *=================================================================
       COPY "LIBLP/LP01BW.CPY" REPLACING LEADING "BW" BY "ORG-BW".
       01  CHANGED-BW-REC     PIC X(BW-SIZE).

       01  ERRCD                       PIC X VALUE " ".

       01  LEGEND          PIC X(70).
       01  LEGEND-NO-WALK       PIC X(35) VALUE
           "F1-CANCEL, ENTER FIELD INFORMATION:".
       01  LEGEND-NO-WALK-SCAN  PIC X(44) VALUE
           "F1-CANCEL, F6-SCAN, ENTER FIELD INFORMATION:".

       01  AGE-MESSAGE.
           03  FILLER                PIC X(43) VALUE
           "DATE OF BIRTH ENTERED MAKES BORROWER < 18. ".
           03  FILLER                PIC X(18) VALUE
           "PLEASE VERIFY DOB!".

       01  LEGEND-01.
           03  FILLER      PIC X(41)  VALUE
           " F1, SELECT SCAN METHOD( 1-SS#, 2-NAME, 3".
           03  FILLER      PIC X(21)  VALUE
           "-SP SS#, 4-SP NAME ):".
       01  LEGEND-02.
           03  FILLER      PIC X(45)  VALUE
           "F1-CANCEL, DELETE, F2-MEMO, F3-LNS, FIELD #, ".
           03  FILLER      PIC X(20)  VALUE
           "P#, <ENTER>-UPDATE: ".
       01  LEGEND-02-WORLD.
           03  FILLER      PIC X(49)  VALUE
           "F1-CANCL, DELETE, F2-MEMO, F3-LNS, F4-CUST INFO, ".
           03  FILLER      PIC X(19)  VALUE
           "FLD #, P#, RTN-UPD:".
       01  LEGEND-SCAN-TYPE-1.
           03  FILLER      PIC X(41)  VALUE
           " F1, ^-CHANGE SCAN METHOD, F6-SCAN, ENTER".
           03  FILLER      PIC X(18)  VALUE
           " BORR (SSNO), RTN:".
       01  LEGEND-SCAN-TYPE-2.
           03  FILLER      PIC X(41)  VALUE
           " F1, ^-CHANGE SCAN METHOD, F6-SCAN, ENTER".
           03  FILLER      PIC X(24)  VALUE
           " BORR (LAST,FIRST), RTN:".
       01  LEGEND-SCAN-TYPE-3.
           03  FILLER      PIC X(41)  VALUE
           " F1, ^-CHANGE SCAN METHOD, F6-SCAN, ENTER".
           03  FILLER      PIC X(20)  VALUE
           " SPOUSE (SSNO), RTN:".
       01  LEGEND-SCAN-TYPE-4.
           03  FILLER      PIC X(41)  VALUE
           " F1, ^-CHANGE SCAN METHOD, F6-SCAN, ENTER".
           03  FILLER      PIC X(26)  VALUE
           " SPOUSE (LAST,FIRST), RTN:".
       01  SCAN-LIST       PIC X(18) VALUE "SCAN TYPE SORTKEY.".
       01  SCAN-BUFFER.
           03  FILLER      PIC X(5)  VALUE "SCAN:".
           03  FILLER      PIC X     VALUE "2".
           03  FILLER      PIC X(24) VALUE ALL "_".
       01  FIRST-SCAN      PIC 9      COMP  VALUE 0.

       01  ZIPCODE-FLAG    PIC X.
       01  TEST-ZIPCODE    PIC X(10).
       01  TEST-ZIP-GROUPS REDEFINES TEST-ZIPCODE.
           03  TEST-ZIP-5    PIC X(5).
           03  TEST-ZIP-DASH PIC X.
           03  TEST-ZIP-4    PIC X(4).

       01  DO-STATE-CY-EXIST-FG  PIC X.
       01  ALLOW-CITY-SCAN-FG    PIC X.
       01  DO-STATE-CT-EXIST-FG  PIC X.
       01  ALLOW-COUNTY-SCAN-FG  PIC X.
       01  ALLOW-BW-UPDATE-FG   PIC X  VALUE "N".
           88 ALLOW-BW-UPDATE-FG-OK VALUE "Y".
           88 ALLOW-BW-UPDATE-FG-BAD VALUE "N".

       01  TEST-PHONE          PIC 9(10).
           88  BAD-PHONE       VALUE 0001111111 1111111111
                                     0002222222 2222222222
                                     0003333333 3333333333
                                     0004444444 4444444444
                                     0005555555 5555555555
                                     0006666666 6666666666
                                     0007777777 7777777777
                                     0008888888 8888888888
                                     0009999999 9999999999
                                     0000000000.
       01  WORK-NAME       PIC X(16).
       01  FILLER          REDEFINES WORK-NAME.
           03  NAME-CHAR   PIC X OCCURS 16 TIMES.
       01  TEST-CHAR       PIC X.
           88 VALID-CHAR   VALUE " " "-" "&" "'".
       01  BAD-NAME-FG     PIC X    VALUE "N".

       01  CHAN            PIC X(4).
       01  HOLD-SYS-DATE   PIC 9(8).
       01  BRANCH          PIC 9(4)   VALUE 0.
       01  SUB             PIC 999     VALUE 0.
       01  SUB2            PIC 999     VALUE 0.
       01  TYP             PIC 9       VALUE 0.
       01  ONE             PIC S9.
       01  ELE             PIC 99.
       01  DEFAULT-SPOUSE  PIC X      VALUE "N".
       01  SPOUSE-RECORD   PIC X      VALUE "N".
       01  SPOUSE-FLAG     PIC X      VALUE "N".

       01  SAVE-SPSSNO     PIC 9(9)   COMP.
       01  SAVE-BUF            VALUE " ".
           03  SAVE-SPLNAME    PIC X(16).
           03  SAVE-SPFNAME    PIC X(16).
           03  SAVE-ADDR1      PIC X(24).
           03  SAVE-ADDR2      PIC X(24).
           03  SAVE-CITY       PIC X(15).
           03  SAVE-STATE      PIC XX.
           03  SAVE-ZIP        PIC X(10).
           03  SAVE-PHONE      PIC X(12).
           03  SAVE-REFDATE    PIC 99/99/99.
           03  SAVE-SP-BDATE   PIC 99/99/9999.
           03  SAVE-LNAME      PIC X(16).
           03  SAVE-FNAME      PIC X(16).
           03  SAVE-SSNO       PIC X(11).
           03  SAVE-BDATE      PIC 99/99/9999.
       01  SAVE-LIST           PIC X(56) VALUE
           "010 020 030 040 050 060 061 070 090 100 340 350 360 370.".


       01  ACTION.
           03  CHKEY       PIC X(4).
           03  CHNO        PIC 9(2).
       01  FORM-NUM1       REDEFINES ACTION.
           03  FORM-KEY1   PIC X(4).
           03  FORM-P1     PIC X.
           03  FORM-SEL1   PIC 9.

       01  ALLOW-CHANGES   PIC X      VALUE "Y".
       01  IO-TYPE         PIC X(3).
       01  HOLD-IO-FG      PIC 99.
       01  NEW-FLAG        PIC 9.
       01  SPOUSE-EXISTS   PIC X VALUE "N".

       01  HOLD-SP-BDATE   PIC 9(8)   COMP-3.
       01  HOLD-BDATE      PIC 9(8)   COMP-3.
       01  SAVE-BWSSNO     PIC 9(9)   VALUE 0.

       01  NAME            PIC X(16).
      * NOT CHANGING HOLD-KEY TO INCLUDE BRANCH
       01  HOLD-KEY        PIC 9(9)   COMP-3.

       01  ALP-BW.
           03  ALP-BW123   PIC X(3).
           03  ALP-HYPHEN1 PIC X.
           03  ALP-BW45    PIC X(2).
           03  ALP-HYPHEN2 PIC X.
           03  ALP-BW6789  PIC X(4).
       01  NUM-BW          PIC 9(9).
       01  NUM-BWX REDEFINES NUM-BW.
           03  NUM-BW123   PIC X(3).
           03  NUM-BW45    PIC X(2).
           03  NUM-BW6789  PIC X(4).
       01  BW-ALP-TABLE.
           03  BW-ALP-TBL  OCCURS 70 TIMES
                           PIC X.
       01  TEST-SSNO       PIC 9(9).
      *************************************
      *   SCREEN DISPLAY BUFFER
      *************************************
       01  DISPLAY-BUF     VALUE SPACES.
           03  F01         PIC X(16).
           03  F02         PIC X(16).
           03  F03         PIC X(24).
           03  F04         PIC X(24).
           03  F05         PIC X(15).
           03  F051        PIC X(4).
           03  F052        PIC X.
           03  F06         PIC XX.
           03  F061         PIC X(10).
           03  F062        PIC X.
           03  F07         PIC X(12).
           03  F071        PIC X.
           03  F08         PIC X(12).
           03  F081        PIC X.
           03  F09         PIC 99/99/99 BLANK ZERO.
           03  F10         PIC 99/99/9999 BLANK ZERO.
           03  F11         PIC X(4).
           03  F12         PIC ZZ.
           03  F13         PIC X(4).
           03  F14         PIC X(4).
           03  F141        PIC X(2).
           03  F15         PIC ZZZ,ZZZ.99.
           03  F16         PIC X(4).
           03  F161        PIC ZZZ9.
           03  F17         PIC X(6).
           03  F18         PIC X(10).

           03  F19         PIC 9(4).
           03  F20         PIC XXX.
           03  F21         PIC XXX.

           03  F22         PIC ZZZZ.
           03  F23         PIC X(4).
           03  F24         PIC X.
           03  F25         PIC X.
           03  F26         PIC X(24).
           03  F27         PIC X(24).
           03  F28         PIC X(24).
           03  F29         PIC X(24).
           03  F30         PIC X(12).
           03  F301        PIC ZZZZ.
           03  F302        PIC X.
           03  F31         PIC ZZ.
           03  F311        PIC X(9).
           03  F32         PIC ZZZ,ZZZ.ZZ.
           03  F33         PIC ZZZ,ZZZ.ZZ.
           03  F331        PIC X(8).

           03  F34         PIC X(16).
           03  F35         PIC X(16).
           03  F36         PIC 999/99/9999.
           03  F37         PIC 99/99/9999 BLANK ZERO.
           03  F38         PIC X.
           03  F39         PIC X.

       01  DISPLAY-LIST.
           03  FILLER      PIC X(56) VALUE
           "010 020 030 040 050 051 052 060 061 062 070 071 080 081 ".
           03  FILLER      PIC X(56) VALUE
           "090 100 110 120 130 140 141 150 160 161 170 180 190 200 ".
           03  FILLER      PIC X(56) VALUE
           "210 220 230 240 250 260 270 280 290 300 301 302 310 311 ".
           03  FILLER      PIC X(36) VALUE
           "320 330 331 340 350 360 370 380 390.".

       01  MAX             PIC 99   VALUE 39.


      *    **********************************
      *    *   ELEMENT SPECIFICATIONS
      *    **********************************
       01  SPEC-TABLE.
      * 1. BORROWER LAST NAME
           03  FILLER      PIC X(8) VALUE "E  A160 ".
      * 2. BORROWER FIRST NAME
           03  FILLER      PIC X(8) VALUE "E  A160 ".
      * 3. ADDRESS 1
           03  FILLER      PIC X(8) VALUE "E  A240 ".
      * 4. ADDRESS 2
           03  FILLER      PIC X(8) VALUE "E  A240 ".
      * 5. CITY
           03  FILLER      PIC X(8) VALUE "E  A150 ".
      * 6. STATE
           03  FILLER      PIC X(8) VALUE "E  A020 ".
      * 7. PHONE
           03  FILLER      PIC X(8) VALUE "E  A120 ".
      * 8. CELL  PHONE
           03  FILLER      PIC X(8) VALUE "E  A120 ".
      * 9. REFERENCE DATE
           03  FILLER      PIC X(8) VALUE "ERNM060 ".
      *10. BIRTHDATE ONLY BDATE
           03  FILLER      PIC X(8) VALUE "ERND080 ".
      *11. ORIGIN
           03  FILLER      PIC X(8) VALUE "E  A040 ".
      *12. DEPENDENTS
           03  FILLER      PIC X(8) VALUE "ENNN020 ".
      *13. RENT OR OWN CODE
           03  FILLER      PIC X(8) VALUE "E  A040 ".
      *14. TIME AT ADDRESS
           03  FILLER      PIC X(8) VALUE "E  A040 ".
      *15. CREDIT-LIMIT
           03  FILLER      PIC X(8) VALUE "E NN062 ".
      *16. CREDIT RATING
           03  FILLER      PIC X(8) VALUE "E  A040 ".
      *17. S.M.S.A. CODE
           03  FILLER      PIC X(8) VALUE "E  A060 ".
      *18. CENSUS TRACK CODE
           03  FILLER      PIC X(8) VALUE "E  A100 ".
      *19. BRANCH OF BORROWER
           03  FILLER      PIC X(8) VALUE "ENNN040N".
      *20. COUNTY CODE
           03  FILLER      PIC X(8) VALUE "E  A030 ".
      *21. CITY TAX CODE
           03  FILLER      PIC X(8) VALUE "E  A030 ".
      *22. ORIGINAL SALESMAN
           03  FILLER      PIC X(8) VALUE "ENNN040 ".
      *23. REP CODE
      *--->03  FILLER      PIC X(8) VALUE "ENNN040 ".
           03  FILLER      PIC X(8) VALUE "E  A040 ".
      *24. SOLICIT CODE
           03  FILLER      PIC X(8) VALUE "E  A010 ".
      *25. CONFIDENTIAL
           03  FILLER      PIC X(8) VALUE "E  A010 ".
      *26. EMPLOYER'S NAME
           03  FILLER      PIC X(8) VALUE "E  A240 ".
      *27. EMPLOYER'S ADDRESS 1
           03  FILLER      PIC X(8) VALUE "E  A240 ".
      *28. EMPLOYER'S DDRESS 2
           03  FILLER      PIC X(8) VALUE "E  A240 ".
      *29. EMPLOYMENT POSITION
           03  FILLER      PIC X(8) VALUE "E  A240 ".
      *30. EMPLOYMENT PHONE NUMBER
           03  FILLER      PIC X(8) VALUE "E  A120 ".
      *31. YEARS OF EMPLOYMENT
           03  FILLER      PIC X(8) VALUE "ENNN020 ".
      *32. NET INCOME MONTHLY
           03  FILLER      PIC X(8) VALUE "E NN062 ".
      *33. OTHER INCOME MONTHLY
           03  FILLER      PIC X(8) VALUE "E NN062 ".
      *34. SPOUSE'S LAST NAME
           03  FILLER      PIC X(8) VALUE "E  A160 ".
      *35. SPOUSE'S FIRST NAME
           03  FILLER      PIC X(8) VALUE "E  A160 ".
      *36. SPOUSE'S SOCIAL SECURITY NUMBER
           03  FILLER      PIC X(8) VALUE "ERNX090 ".
      *37. SPOUSE'S BIRTH DATE ONLY BDATE
           03  FILLER      PIC X(8) VALUE "ERND080 ".
      *38. PRIVACY OPT OUT FLAG (WORLD: TEXTING OPTION)
           03  FILLER      PIC X(8) VALUE "E  A010 ".
      *39. CEASE AND DESIST TYPE
           03  FILLER      PIC X(8) VALUE "E  A010N".
       01  SPEC-TAB        REDEFINES SPEC-TABLE.
           03  SPEC-REC    OCCURS 39 TIMES.
               05  SPEC    PIC X(7).
               05  CHFG    PIC X.


       01  LEGEND-WALK.
           03  FILLER             PIC X(17)  VALUE
               "F1-CANCEL, ^-UP, ".
           03  FILLER             PIC X      VALUE X'76'.
           03  FILLER             PIC X(31)  VALUE
               "-DOWN, ENTER FIELD INFORMATION:".
       01  LEGEND-WALK-SCAN.
           03  FILLER                    PIC X(26) VALUE
               "F1-CANCEL, F6-SCAN, ^-UP, ".
           03  FILLER             PIC X      VALUE X'76'.
           03  FILLER                    PIC X(31) VALUE
               "-DOWN, ENTER FIELD INFORMATION:".

       COPY "LIBWI/PFSWINDOWSW.CPY".
       COPY "LIBCL/CLMENUW_EXT.CPY".
       COPY "LIBWI/CDSCANW.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBLP/LPWKPATH.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/GPENVW.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBWI/MEMOSCW.CPY".
       COPY "LIBWI/BWSCANW.CPY".
       COPY "LIBWI/BWLNS2W.CPY".
       COPY "LIBWI/CYSCANW.CPY".
       COPY "LIBWI/CTSCANW.CPY".
       COPY "LIBWI/CISCANW.CPY".
       COPY "LIBGB/PASSWDW.CPY".
       78  CRSCORE-SEQ   VALUE 0.
       78  REFDATE-SEQ   VALUE 1.
       78  CONFID-SEQ    VALUE 2.
      * THIS ONE PREVENTS REMOVING IT (OPTOUT)
       78  OPT-OUT-SEQ   VALUE 3.
       78  SMSA-SEQ      VALUE 4.
       78  LNAME-SEQ     VALUE 5.
       78  SOLICIT-SEQ   VALUE 6.
      * THIS ONE DOES NOT ALLOW ANY CHANGE
       78  OPT-OUT-SEQ2  VALUE 7.
       78  REFDATE-SEQ2  VALUE 8.
       78  NO-SPACES-SEQ VALUE 9.
       78  MX-NAME-RFC   VALUE 10.
       78  REPCODE-SEQ   VALUE 11.
       78  FNAME-SEQ     VALUE 12.
      * ALL SPOUSE FIELDS(34 - 37) AND CITY CODE (21)
       78  CITY-SEQ      VALUE 13.
       78  BDATE-SEQ     VALUE 14.
       78  18-22-SEQ           VALUE 15.
      * BWCRE2
       78  PHONE-SEQ      VALUE 16.
       78  CEASE-DESIST-SEQ VALUE 17.

       COPY "LIBGB/PHON400.CPY".
       COPY "LIBLP/LPBAGEW.CPY".
       01  BI-STAT                PIC XX.
       01  BI-LN-ACCTNO           PIC 9(8).
       01  BI-LN-BWTABLE.
           03  BI-LN-BORRTBL      OCCURS 4 TIMES.
               05  BI-LN-SSNO     PIC 9(9).
               05  BI-LN-MAKERCD  PIC X.
       01  BI-LN-OWNBR            PIC 9999.
       01  BI-LN-ORGST            PIC XX.
       01  BI-LN-SALEFIN          PIC X.
       01  BI-LN-GIFTNET          PIC X.
       01  BI-LN-ENTDATE          PIC 9(8)      COMP-3.
       01  BI-LN-POFFDATE         PIC 9(8)      COMP-3.
       01  BI-LN-PLCD             PIC X.
       01  BI-LN-CONVERCD         PIC X.
       01  BI-CHTO-SSNO           PIC 9(9).
       01  BI-LN-SMALLOAN         PIC X.
       01  BI-LN-PLDATE           PIC 9(8)      COMP-3.

       01  MODBOR-WINDOW-HANDLE        PIC X(10).
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBWI/MODBOR_DEF.CPY".
       COPY "LIBWI/CEASED_DEF.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBWI/MODBOR_WKS.CPY".
       COPY "LIBWI/CEASED_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".

       COPY "LIBGB/GETFMW.CPY".

       SCREEN SECTION.
       COPY "LIBWI/MODBOR_SCN.CPY".
       COPY "LIBWI/CEASED_SCN.CPY".

       PROCEDURE DIVISION
             USING FORM-PATHNAME EXIT-PATHNAME.

      ************************************
      *   P R O G R A M   C O N T R O L  *
      ************************************
       MAIN-PROGRAM SECTION.

           PERFORM SQL-CONNECT.

      D    STOP MESS.
           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "MODBOR" TO VDU-FORM-NAME.

       COPY "LIBGB/CHKSEC.CPY".

           MOVE " " TO VDU-WIMENU-SELECTION.

           MOVE EXT-CONAME-DAILY-PASSWORD TO CHAN.

           MOVE EXT-CONAME-USER-LOC TO BR-NO BRANCH.
           PERFORM OPEN-BR-FILE.
           PERFORM READ-BR-FILE.
           PERFORM CLOSE-BR-FILE.

           DISPLAY FLOATING GRAPHICAL WINDOW
                   AT  SCREEN LINE MODBOR-BEGIN-X * VDU-WIN-L-OFFSET
                   AT  SCREEN COL  MODBOR-BEGIN-Y * VDU-WIN-C-OFFSET
                   CELL SIZE IS LABEL FONT
           COLOR IS VDU-WCOLOR-TCLP,CONTROL VALUE IS VDU-WCTRL-TCLP
                   SIZE    MODBOR-COLUMNS + VDU-WIN-W-MOD
                   LINES   MODBOR-LINES + VDU-WIN-H-MOD
                   BLANK SCREEN
                   HANDLE IS MODBOR-WINDOW-HANDLE.

           MOVE EXT-CONAME-SYSDATE TO ALP-DATE.
           PERFORM CONVERT-ALPDATE-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD TO HOLD-SYS-DATE.

           MOVE "S  A180TITLE:FR." TO VDU.
           MOVE " BORROWER CHANGES" TO VDUBUF.
           PERFORM SCREENIO.

           PERFORM GET-GPENV.

       MAIN-MODULE.
           PERFORM FORM-RESET.

           PERFORM REC-BW-MODULE.
           IF IO-TYPE = "END"
              GO TO END-ROUTINE.
           IF IO-TYPE = "CLR"
              GO TO MAIN-MODULE.

           IF BW-IS-CEASE-DESIST
              PERFORM CEASE-POP-UP.

           PERFORM ENTRY-MODULE.
           IF IO-TYPE = "RWR"
              PERFORM WRITE-MODULE
              GO TO MAIN-MODULE.
           IF IO-TYPE = "DEL"
              PERFORM DELETE-MODULE.
           IF IO-TYPE = "END"
              GO TO END-ROUTINE.

           GO TO MAIN-MODULE.

      **************************************
       REC-BW-MODULE SECTION.
      **************************************
       REC-BW-MODULE-NO.
           MOVE SPACES TO IO-TYPE.
           MOVE 0 TO NEW-FLAG.

           IF DEFAULT-SPOUSE = "Y"
              MOVE SAVE-SPSSNO TO BW-SSNO NUM-BW
              PERFORM NUMBW-TO-ALPBW
              MOVE "S  A110BWNO." TO VDU
              MOVE ALP-BW TO VDUBUF
              PERFORM SCREENIO
              GO TO READ-BWFILE.

           MOVE "F7-EXIT, F5 OR F6-SCAN, ENTER SOC-SEC-NO:"
                                                    TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "S  A000BWP." TO VDU.
           MOVE "BORROWER:" TO VDUBUF.
           PERFORM SCREENIO.

       REC-BW-SSNO.
           MOVE "ERNX090BWNO." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>"
              MOVE "END" TO IO-TYPE
              GO TO END-REC-BW-MODULE.
           IF VDUSTATUS = "1:"
              PERFORM ENTER-SCANKEY
              GO TO END-REC-BW-MODULE.
           IF VDUSTATUS = "1<"
              PERFORM BORROWER-SCAN
              IF IO-FG = 0
                 GO TO READ-BWFILE
              ELSE
                 GO TO REC-BW-SSNO.
           IF VDUSTATUS NOT = "00"
              GO TO REC-BW-SSNO.

           MOVE VDUNUM0 TO BW-SSNO NUM-BW.
           IF BW-SSNO = 0
              GO TO REC-BW-SSNO.

       READ-BWFILE.
           MOVE NUM-BW TO BW-SSNO
                          HOLD-KEY.
           PERFORM OPEN-BW1-FILE.
           PERFORM READ-BW1-FILE.
           PERFORM CLOSE-BW1-FILE.
           IF IO-FG = 0
              MOVE BW-REC TO ORG-BW-REC
              PERFORM DISPLAY-FIELDS
              MOVE 0 TO NEW-FLAG
           ELSE
              IF (NUM-BW NOT = 999999999)
                 PERFORM NEW-RECORD-SETUP
              ELSE
                 GO TO REC-BW-MODULE-NO.

           IF DEFAULT-SPOUSE = "Y"
              MOVE "N" TO DEFAULT-SPOUSE
              MOVE "Y" TO SPOUSE-RECORD
              MOVE SAVE-SPLNAME TO BW-LNAME
              MOVE SAVE-SPFNAME TO BW-FNAME
              MOVE SAVE-ADDR1 TO BW-ADR1
              MOVE SAVE-ADDR2 TO BW-ADR2
              MOVE SAVE-CITY TO BW-CITY
              MOVE SAVE-STATE TO BW-STATE
              MOVE SAVE-ZIP TO BW-ZIP
              MOVE SAVE-PHONE TO VDUBUF
              PERFORM ENTER-PHONE
              MOVE TEL-BUF TO BW-PHONE
              MOVE SAVE-REFDATE TO ALP-DATE
              PERFORM CONVERT-ALPDATE-TO-YYYYMMDD
              MOVE DATE-YYYYMMDD TO BW-REFDATE
              MOVE HOLD-SP-BDATE TO BW-BDATE
              MOVE SAVE-LNAME TO BW-SPLNAME
              MOVE SAVE-FNAME TO BW-SPFNAME
              MOVE SAVE-SSNO TO ALP-BW
              PERFORM ALPBW-TO-NUMBW
              MOVE NUM-BW TO BW-SPSSNO
              MOVE HOLD-BDATE TO BW-SP-BDATE
              MOVE "N" TO BW-SOLICIT
              MOVE SAVE-LIST TO WR-LIST
              MOVE SAVE-BUF TO WR-BUF
              PERFORM CALL-WRFORM.

       END-REC-BW-MODULE.
           EXIT.

      ****************************************
       NEW-RECORD-SETUP SECTION.
      ****************************************
           MOVE 1 TO NEW-FLAG.
           MOVE "S  A000NEW." TO VDU.
           MOVE "*** NEW RECORD ***" TO VDUBUF.
           PERFORM SCREENIO.
       CLEAR-NEW-RECORD.
           PERFORM CLEAR-BW-FILE.

           MOVE BW-PATH-OWNBR TO BW-OWNBR.
           MOVE HOLD-KEY      TO BW-SSNO.
           MOVE "N" TO BW-CONFIDENTIAL BW-BAD-ADR-FG
                       BW-BAD-PHONE-FG.
           MOVE "Y" TO BW-SOLICIT.
           MOVE "S  A020060." TO VDU.
           MOVE BR-STATE TO BW-STATE VDUBUF.
           PERFORM SCREENIO.

      * DONT STOP ON BRANCH
           MOVE "SN N040190." TO VDU.
           MOVE BRANCH TO BW-ORGBR VDUNUM0. 
           PERFORM SCREENIO.
           MOVE "N" TO CHFG(19).

           MOVE 0 TO ELE.

      ***********************************
       ARRANGE-BW-KEY SECTION.
      ***********************************
           MOVE 1 TO VDU-I VDU-J.
           MOVE SPACES TO BW-ALP-TABLE.
       ARR-BW-KEY-AGAIN.
           IF VDU-I NOT < 35
              GO TO ARR-BW-KEY-EXIT.
           IF VDUTAB(VDU-I) NOT = ","
              MOVE VDUTAB(VDU-I) TO BW-ALP-TBL(VDU-J)
            ELSE
              MOVE 35 TO VDU-J.
           ADD 1 TO VDU-I.
           ADD 1 TO VDU-J.
           GO TO ARR-BW-KEY-AGAIN.
       ARR-BW-KEY-EXIT.
           EXIT.

      **********************************************
       BORROWER-SCAN SECTION.
      **********************************************
           MOVE 0          TO BWSCAN-NO.
           MOVE "S"        TO BWSCAN-SCAN-TYPE.
           MOVE "WI/BWSCAN" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH BWSCAN-BUF EXIT-PATHNAME.
           CANCEL FORM-PROGX.
           IF BWSCAN-STATUS NOT = "00" OR BWSCAN-NO = 0
              MOVE 9 TO IO-FG
              GO TO EXIT-BORROWER-SCAN.

           MOVE BWSCAN-NO TO BW-SSNO NUM-BW HOLD-KEY.
           MOVE "S  X110BWNO." TO VDU.
           MOVE BW-SSNO TO VDU-SSNO-EDIT.
           PERFORM SCREENIO.

           MOVE BW-SSNO TO VDU-SSNO-EDIT.
           MOVE 0 TO IO-FG.
       EXIT-BORROWER-SCAN.
           EXIT.

      *************************************************
      *    ENTER SORTKEY ROUTINES
      *    TYPE = 1 - BORROWER SOCIAL SEC. #
      *           2 - BORROWER NAME
      *           3 - SPOUSE SOCIAL SEC. #
      *           4 - SPOUSE NAME
      *************************************************
       ENTER-SCANKEY SECTION.

           MOVE 2 TO TYP.

           PERFORM OPEN-BW2-FILE.
           MOVE BW-PATH-OWNBR TO BW-OWNBR.
           MOVE SPACES        TO BW-NAME.

           PERFORM START-BW2-FILE.

           MOVE SCAN-BUFFER TO WR-BUF.
           MOVE SCAN-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

           GO TO ENTER-KEY.

       ENTER-TYPE.
           MOVE
            "F1, 1-BORR SS#, 2-BORR NAME, 3-SPOUSE SS#, 4-SPOUSE NAME:"
                                                             TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "EN N010TYPE." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO CLOSE-SCANKEY.
           IF VDUSTATUS NOT = "00"
              GO TO ENTER-TYPE.
           MOVE VDUNUM0 TO TYP.
           IF TYP < 1 OR TYP > 4
              GO TO ENTER-TYPE.

           PERFORM OPEN-BW1-FILE.

           MOVE BW-PATH-OWNBR TO BW-OWNBR.

           IF TYP = 1
              MOVE 0 TO BW-SSNO
              PERFORM START-BW1-FILE
           ELSE
              IF TYP = 2
                 MOVE SPACES TO BW-NAME
                 PERFORM START-BW2-FILE
              ELSE
                 IF TYP = 3
                    MOVE 0 TO BW-SPSSNO
                    PERFORM START-BW3-FILE
                 ELSE
                    IF TYP = 4
                       MOVE SPACES TO BW-SPNAME
                       PERFORM START-BW4-FILE.

       ENTER-KEY.
           IF TYP = 1
              MOVE LEGEND-SCAN-TYPE-1 TO LEGEND
           ELSE
              IF TYP = 2
                 MOVE LEGEND-SCAN-TYPE-2 TO LEGEND
              ELSE
                 IF TYP = 3
                    MOVE LEGEND-SCAN-TYPE-3 TO LEGEND
                 ELSE
                    IF TYP = 4
                       MOVE LEGEND-SCAN-TYPE-4 TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "E RA240SORTKEY." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO CLOSE-SCANKEY.
           IF VDUSTATUS = "1U"
              PERFORM CLOSE-SCANKEY
              GO TO ENTER-TYPE.
           IF VDUSTATUS = "1<"
              GO TO READ-FILES-NEXT.
           IF VDUSTATUS NOT = "00"
              GO TO ENTER-KEY.

           IF VDUBUF = " "
              IF FIRST-SCAN NOT = 0
                 MOVE BW-REC TO ORG-BW-REC
                 GO TO CLOSE-SCANKEY
              ELSE
                 GO TO ENTER-KEY.
           IF TYP = 1 OR TYP = 3
              INSPECT VDUBUF REPLACING ALL " " BY "0"
              MOVE VDUBUF TO TEST-SSNO
              IF TEST-SSNO NOT NUMERIC
              GO TO ENTER-KEY.
           IF TYP = 1 OR 3
              MOVE VDUBUF TO BW-SSNO BW-SPSSNO
           ELSE
              IF TYP = 2 OR 4
                 PERFORM ARRANGE-BW-KEY
                 MOVE BW-ALP-TABLE TO BW-NAME BW-SPNAME.
           IF TYP = 1
              PERFORM START-BW1-FILE
           ELSE
              IF TYP = 2
                 PERFORM START-BW2-FILE
              ELSE
                 IF TYP = 3
                    PERFORM START-BW3-FILE
                 ELSE
                    IF TYP = 4
                       PERFORM START-BW4-FILE.
       READ-FILES-NEXT.
           IF TYP = 1
              PERFORM READ-BW1-FILE-NEXT
      *       UNLOCK BW-FILE
           ELSE
              IF TYP = 2
                 PERFORM READ-BW2-FILE-NEXT
              ELSE
                 IF TYP = 3
                    PERFORM READ-BW3-FILE-NEXT
                 ELSE
                    IF TYP = 4
                       PERFORM READ-BW4-FILE-NEXT.

           IF (IO-FG = 9) OR (BW-OWNBR NOT = BW-PATH-OWNBR)
              MOVE "CLR" TO IO-TYPE
              GO TO CLOSE-SCANKEY.

           MOVE "S  X110BWNO." TO VDU.
           MOVE BW-SSNO TO HOLD-KEY
                           NUM-BW.
           MOVE BW-SSNO TO VDU-SSNO-EDIT.
           PERFORM SCREENIO.

           PERFORM DISPLAY-FIELDS.

           MOVE 1 TO FIRST-SCAN.
           GO TO ENTER-KEY.

       CLOSE-SCANKEY.
           PERFORM CLOSE-BW1-FILE.

       END-SCANKEY.
           MOVE 0 TO FIRST-SCAN.
           MOVE SPACES    TO WR-BUF.
           MOVE SCAN-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
       EXIT-ENTER-SCANKEY.
           EXIT.

      ************************************************
      *   ENTRY-MODULE
      *   NEW-FLAG = 0 - MODIFY EXISTING RECORD
      *              1 - INITIAL NEW RECORD ENTRY
      *              2 - MODIFY NEW RECORD
      ************************************************
       ENTRY-MODULE SECTION.
           MOVE SPACES TO IO-TYPE.
           MOVE 1 TO ONE.
           IF SPOUSE-FLAG = "Y"
              MOVE "N" TO CHFG(9) CHFG(10)
                          CHFG(34) CHFG(35) CHFG(36) CHFG(37)
              MOVE 10 TO ELE
              MOVE "....FINISH BUILDING THE SPOUSE'S RECORD!" TO LEGEND
              PERFORM SEND-LEGEND
           ELSE
              MOVE " " TO CHFG(9) CHFG(10)
                          CHFG(34) CHFG(35) CHFG(36) CHFG(37)
              MOVE 0 TO ELE.
       CREATE-ROUTINE.
           IF NEW-FLAG NOT = 0
              MOVE "N" TO CHFG(23).

           IF NEW-FLAG NOT = 1
              GO TO MODIFY-ROUTINE.
           ADD ONE TO ELE.
           IF ELE < 1
              MOVE 1 TO ELE ONE.
           IF SPOUSE-FLAG = "Y"
              IF ELE < 34
                 GO TO ENTER-ELE.
           IF ELE NOT > MAX
              GO TO ENTER-ELE.
           MOVE 2 TO NEW-FLAG.

       MODIFY-ROUTINE.
           MOVE " " TO CHFG(19).
           MOVE " " TO CHFG(38).
           IF NEW-FLAG = 0
              MOVE " " TO CHFG(23).
           MOVE LEGEND-02-WORLD TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "R RA060SEL." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF VDUSTATUS = "12"
              MOVE "B"           TO MEMOSC-ONLY
              MOVE BW-PATH-OWNBR TO EXT-WI-CL-OWNBR
              MOVE 1             TO EXT-WI-CL-SSNO-NO
              MOVE BW-SSNO       TO EXT-WI-CL-SSNO(1)
              MOVE HOLD-SYS-DATE TO EXT-WI-CL-TRANS-DATE
              MOVE "WI/MEMOSC" TO FORM-NAM
              CALL FORM-PROGX USING FORM-PATH MEMOSC-BUF EXIT-PATHNAME
              CANCEL FORM-PROGX
              GO TO MODIFY-ROUTINE.
           IF VDUSTATUS = "16"
              MOVE "Y"     TO BWLNS2-SCAN-ONLY
              MOVE BW-SSNO TO BWLNS2-KEY
              MOVE HOLD-SYS-DATE TO DATE-YYYYMMDD
              PERFORM CONVERT-YYYYMMDD-TO-ALPDATE
              MOVE ALP-DATE TO BWLNS2-SYSDATE
              MOVE "WI/BWLNS2" TO FORM-NAM
              CALL FORM-PROGX USING FORM-PATH BWLNS2-BUF EXIT-PATHNAME
              CANCEL FORM-PROGX
              GO TO MODIFY-ROUTINE.

           IF VDUSTATUS = "18"
              MOVE "WI/CISCAN" TO FORM-NAM
              CALL FORM-PROGX USING FORM-PATH CISCAN-BUF
                                    EXIT-PATHNAME
              CANCEL FORM-PROGX
              GO TO MODIFY-ROUTINE.
           IF VDUSTATUS NOT = "00"
              GO TO MODIFY-ROUTINE.
           MOVE VDUBUF TO ACTION.
           IF FORM-KEY1 = " "
              IF FORM-P1 = "P"
                 IF FORM-SEL1 NUMERIC
                    IF FORM-SEL1 = 2
                       PERFORM CALL-BWCRE2
                       GO TO MODIFY-ROUTINE
                    ELSE
                    IF FORM-SEL1 = 3
                       PERFORM CALL-BWCRE3
                       GO TO MODIFY-ROUTINE
                    ELSE
                    IF FORM-SEL1 = 4
                       PERFORM CALL-BWCRE4
                       GO TO MODIFY-ROUTINE
                    ELSE
                    IF FORM-SEL1 = 5
                       PERFORM CALL-BWCRE5
                       GO TO MODIFY-ROUTINE
                    ELSE
                       GO TO MODIFY-ROUTINE.

           IF ACTION = SPACES
              PERFORM VALIDATE-REQUIRED-FIELD
              IF NOT (ALLOW-BW-UPDATE-FG-OK)
                 GO TO MODIFY-ROUTINE
              END-IF
              MOVE "RWR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF ACTION = "DELETE"
              PERFORM VERIFY-DELETE
              IF IO-FG = 9
                 GO TO MODIFY-ROUTINE
              ELSE
                 MOVE "DEL" TO IO-TYPE
                 GO TO END-ENTRY-MODULE.
           INSPECT ACTION REPLACING ALL SPACES BY ZEROS.
           IF CHNO NOT NUMERIC
              GO TO MODIFY-ROUTINE.
           IF CHNO = 0
              GO TO MODIFY-ROUTINE.
           IF CHNO > MAX
              GO TO MODIFY-ROUTINE.
           MOVE CHNO TO ELE.

      * OVERRIDE REFERENCE DATE
           IF ELE = 09
              IF NEW-FLAG = 0
                 MOVE REFDATE-SEQ TO PASSWDW-SEQ
                 MOVE "TO OVERRIDE REFERENCE DATE...."
                                          TO PASSWDW-PROMPT2
                 PERFORM VERIFY-PASSWORD
                 IF PASSWDW-STAT NOT = "OK"
                    GO TO MODIFY-ROUTINE.


      * ADDED FOR MEXICO PR#1316
      * CENSUS(18) AND ORGSLSMN(22), RESTRICT ACCESS IF FIELDS AREN'T SPACES 
      * AND OPEN ACCOUNTS
           IF (ELE = 18) AND (NEW-FLAG = 0) AND (BW-LOANS NOT = 0)
              IF (BW-CENSUSCD NOT = SPACES)
                 MOVE 18-22-SEQ TO PASSWDW-SEQ
                 MOVE "TO CHANGE THIS FIELD ......" TO PASSWDW-PROMPT2
                 PERFORM VERIFY-PASSWORD
                 IF PASSWDW-STAT NOT = "OK"
                    GO TO MODIFY-ROUTINE.

           IF (ELE = 22) AND (NEW-FLAG = 0) AND (BW-LOANS NOT = 0)
              IF (BW-ORGSLSMN NOT = 0)
                 MOVE 18-22-SEQ TO PASSWDW-SEQ
                 MOVE "TO CHANGE THIS FIELD ......" TO PASSWDW-PROMPT2
                 PERFORM VERIFY-PASSWORD
                 IF PASSWDW-STAT NOT = "OK"
                    GO TO MODIFY-ROUTINE.


      *OVERRIDE REP CODE
           IF ELE = 23
              IF NEW-FLAG = 0
                 MOVE REPCODE-SEQ TO PASSWDW-SEQ
                 MOVE "TO OVERRIDE REPCODE .... ."
                                          TO PASSWDW-PROMPT2
                 PERFORM VERIFY-PASSWORD
                 IF PASSWDW-STAT NOT = "OK"
                    GO TO MODIFY-ROUTINE.

      * ALL SPOUSE FIELDS(34 - 37) AND CITY CODE (21)
           IF ELE = 21 AND NEW-FLAG = 0
              MOVE CITY-SEQ TO PASSWDW-SEQ
              MOVE "TO CHANGE THIS FIELD ......" TO PASSWDW-PROMPT2
              PERFORM VERIFY-PASSWORD
              IF PASSWDW-STAT NOT = "OK"
                 GO TO MODIFY-ROUTINE.

      * CHANGE SOLICIT FLAG ON AN EXISTING BORROWER
           IF ELE = 24
              IF NEW-FLAG = 0
                 MOVE SOLICIT-SEQ TO PASSWDW-SEQ
                 MOVE "TO CHANGE CONTACT/SOLICIT FLAG"
                                             TO PASSWDW-PROMPT2
                 PERFORM VERIFY-PASSWORD
                 IF PASSWDW-STAT NOT = "OK"
                    GO TO MODIFY-ROUTINE.

      * CHANGE CONFIDENTIAL FLAG ON AN EXISTING BORROWER
           IF ELE = 25
              IF NEW-FLAG = 0
                 MOVE CONFID-SEQ TO PASSWDW-SEQ
                 MOVE "TO CHANGE CONFIDENTIAL FLAG" TO PASSWDW-PROMPT2
                 PERFORM VERIFY-PASSWORD
                 IF PASSWDW-STAT NOT = "OK"
                    GO TO MODIFY-ROUTINE.

      * CHANGE PRIVACY OPT OUT FLAG ON AN EXISTING BORROWER
           IF ELE = 38
              IF NEW-FLAG = 0
                 MOVE OPT-OUT-SEQ2 TO PASSWDW-SEQ
                 MOVE "TO CHANGE PRIVACY OPTOUT FLAG" TO PASSWDW-PROMPT2
                 PERFORM VERIFY-PASSWORD
                 IF PASSWDW-STAT NOT = "OK"
                    GO TO MODIFY-ROUTINE.

      * CHANGE SMSA (STRTGCD) ON AN EXISTING BORROWER
           IF ELE = 17
              IF NEW-FLAG = 0
                 MOVE SMSA-SEQ TO PASSWDW-SEQ
                 MOVE "TO CHANGE S.M.S.A. CODE" TO PASSWDW-PROMPT2
                 PERFORM VERIFY-PASSWORD
                 IF PASSWDW-STAT NOT = "OK"
                    GO TO MODIFY-ROUTINE.

           IF NEW-FLAG = 0
              IF BW-LOANS > 0
                 IF ELE = 1 OR 2
                    MOVE MX-NAME-RFC TO PASSWDW-SEQ
                    MOVE "TO CHANGE NAME/RFC.." TO PASSWDW-PROMPT2
                    PERFORM VERIFY-PASSWORD
                    IF PASSWDW-STAT NOT = "OK"
                       GO TO MODIFY-ROUTINE.

      * CHANGE LAST NAME ON AN EXISTING BORROWER
           IF ELE = 01
              IF NEW-FLAG = 0
                 MOVE LNAME-SEQ TO PASSWDW-SEQ
                 MOVE "TO CHANGE LAST NAME...." TO PASSWDW-PROMPT2
                 PERFORM VERIFY-PASSWORD
                 IF PASSWDW-STAT NOT = "OK"
                    GO TO MODIFY-ROUTINE.

      * CHANGE FIRST NAME ON AN EXISTING BORROWER
           IF ELE = 02
              IF NEW-FLAG = 0
                 MOVE FNAME-SEQ TO PASSWDW-SEQ
                 MOVE "TO CHANGE FIRST NAME...." TO PASSWDW-PROMPT2
                 PERFORM VERIFY-PASSWORD
                 IF PASSWDW-STAT NOT = "OK"
                    GO TO MODIFY-ROUTINE.

      * CHANGE BIRTHDATE ON AN EXISTING BORROWER
           IF ELE = 10 AND NEW-FLAG = 0
              IF BW-BDATE NOT = 0
                 MOVE BDATE-SEQ TO PASSWDW-SEQ
                 MOVE "TO CHANGE BIRTHDATE...." TO PASSWDW-PROMPT2
                 PERFORM VERIFY-PASSWORD
                 IF PASSWDW-STAT NOT = "OK"
                    GO TO MODIFY-ROUTINE.

      * CEASE & DESIST FIELDS THAT GET SET WITH THE UPDATE
      * CANT CHANGE WITHOUT A PASSWORD
      * HOME PHONE/CELL PHONE
           IF NEW-FLAG = 0
              IF (ELE = 39)
               OR
      * CELL HOME
                ( (ELE = 7 OR 8) AND
                  (BW-CEASE-DESIST-TYPE = "V" OR "W" OR "C") )
               OR
      * EMPLOYER
                 ( (ELE = 26 OR 27 OR 28 OR 29 OR 30) AND
                    (BW-CEASE-DESIST-TYPE = "E" OR "W" OR "C") )
               OR
      * SOLICIT, CONFIDENT, TEXT OPTION
                 ( (ELE = 24 OR 25 OR 38) AND
                    (BW-CEASE-DESIST-TYPE = "W" OR "C") )
                    MOVE CEASE-DESIST-SEQ         TO PASSWDW-SEQ
                    MOVE "TO CHANGE DUE TO CEASE" TO PASSWDW-PROMPT2
                    PERFORM VERIFY-PASSWORD-MUST-EXIST
                    IF PASSWDW-STAT NOT = "OK"
                       GO TO MODIFY-ROUTINE.

       ENTER-ELE.
           IF ELE = 09
              IF NEW-FLAG = 2
                 MOVE " " TO CHFG(ELE)
                 MOVE REFDATE-SEQ2 TO PASSWDW-SEQ
                 MOVE "TO OVERRIDE REFERENCE DATE...."
                                          TO PASSWDW-PROMPT2
                 PERFORM VERIFY-PASSWORD
                 IF PASSWDW-STAT NOT = "OK"
                    GO TO MODIFY-ROUTINE.
           IF CHKEY NOT = ZEROS
              IF CHFG (ELE) = " "
                 IF NEW-FLAG NOT = 1
                    GO TO MODIFY-ROUTINE.
           IF CHFG (ELE) = "N"
              IF NEW-FLAG = 1
                 GO TO CREATE-ROUTINE
              ELSE
                 IF CHKEY NOT = CHAN
                    GO TO MODIFY-ROUTINE.
           IF ELE = 2
              MOVE "N" TO SPOUSE-RECORD.

      *    MOVE " F1-CANCEL, ENTER FIELD INFORMATION:" TO LEGEND.
      *    PERFORM SEND-LEGEND.

           IF NEW-FLAG = 1
              MOVE LEGEND-WALK TO LEGEND
              IF ELE = 13
                 MOVE LEGEND-WALK-SCAN TO LEGEND
              ELSE
                 IF ELE = 21 AND ALLOW-CITY-SCAN-FG = "Y"
                    MOVE LEGEND-WALK-SCAN TO LEGEND
                 ELSE
                    IF ELE = 20 AND ALLOW-COUNTY-SCAN-FG = "Y"
                       MOVE LEGEND-WALK-SCAN TO LEGEND
                    END-IF
                 END-IF
              END-IF
           ELSE
              MOVE LEGEND-NO-WALK TO LEGEND
              IF ELE = 13
                 MOVE LEGEND-NO-WALK-SCAN TO LEGEND
              ELSE
                 IF ELE = 21 AND ALLOW-CITY-SCAN-FG = "Y"
                    MOVE LEGEND-NO-WALK-SCAN TO LEGEND
                 ELSE
                    IF ELE = 20 AND ALLOW-COUNTY-SCAN-FG = "Y"
                       MOVE LEGEND-NO-WALK-SCAN TO LEGEND.

           PERFORM SEND-LEGEND.
           MOVE SPEC (ELE) TO VDU.
           MOVE ELE TO VDUROW.
           MOVE 0 TO VDUCOL.
           MOVE "." TO VDUDECIMAL.
           PERFORM SCREENIO.
       TEST-F4.
           IF VDUSTATUS = "1U"
              IF NEW-FLAG = 1
                 MOVE -1 TO ONE
                 GO TO CREATE-ROUTINE.
       TEST-F5.
           IF VDUSTATUS = "1D"
              MOVE 1 TO ONE
              GO TO GOTO-FIELD.

       TEST-F6.
           IF NOT F6-KEY
              GO TO TEST-F1.

           IF ELE = 11
              MOVE "OR" TO CDSCAN-TYPE
              PERFORM CODE-SCAN
              IF IO-GOOD
                 MOVE 1 TO ONE
                 GO TO GOTO-FIELD
              ELSE
                 GO TO ENTER-ELE.
           IF ELE = 13
              MOVE "BW" TO CDSCAN-TYPE
              PERFORM CODE-SCAN
              IF IO-GOOD
                 MOVE 1 TO ONE
                 GO TO GOTO-FIELD
              ELSE
                 GO TO ENTER-ELE.

           IF ELE = 21 AND ALLOW-CITY-SCAN-FG = "Y"
              PERFORM CYSCAN-SCAN
              IF IO-GOOD
                 MOVE 1 TO ONE
                 GO TO GOTO-FIELD
              ELSE
                 GO TO ENTER-ELE.
           IF ELE = 20 AND ALLOW-CITY-SCAN-FG = "Y"
              PERFORM CTSCAN-SCAN
              IF IO-GOOD
                 MOVE 1 TO ONE
                 GO TO GOTO-FIELD
              ELSE
                 GO TO ENTER-ELE.
       TEST-F1.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.

           IF VDUSTATUS NOT = "00"
              GO TO ENTER-ELE.
           MOVE 1 TO ONE.

       GOTO-FIELD.
           GO TO F-01 F-02 F-03 F-04 F-05
                 F-06 F-07 F-08 F-09 F-10
                 F-11 F-12 F-13 F-14 F-15
                 F-16 F-17 F-18 F-19 F-20
                 F-21 F-22 F-23 F-24 F-25
                 F-26 F-27 F-28 F-29 F-30
                 F-31 F-32 F-33 F-34 F-35
                 F-36 F-37 F-38 F-39
                 DEPENDING ON ELE.
           GO TO END-ENTRY-MODULE.

      *    **********************************
      *    *      SET RECORD ELEMENTS
      *    **********************************
       F-01.
           MOVE VDUBUF TO BW-LNAME.
           IF BW-LNAME = " "
              GO TO ENTER-ELE.
           IF GP-EDIT-BORROWER-NAME-FG = "Y"
              MOVE BW-LNAME TO WORK-NAME
              PERFORM CHECK-BORROWER-NAME
              IF BAD-NAME-FG = "Y"
                 MOVE
                 "VALID CHARACTERS ARE A-Z, 0-9, SPACE, -, ', AND &"
                    TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       F-02.
           MOVE VDUBUF TO BW-FNAME.
           IF GP-EDIT-BORROWER-NAME-FG = "Y"
              MOVE BW-FNAME TO WORK-NAME
              PERFORM CHECK-BORROWER-NAME
              IF BAD-NAME-FG = "Y"
                 MOVE
                 "VALID CHARACTERS ARE A-Z, 0-9, SPACE, -, ', AND &"
                    TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-ELE.
           IF BW-FNAME = " "
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       F-03.
           IF VDUBUF = SPACES
              MOVE NO-SPACES-SEQ TO PASSWDW-SEQ
              MOVE "TO REMOVE ADDRESS....:" TO PASSWDW-PROMPT2
              PERFORM VERIFY-PASSWORD
              IF PASSWDW-STAT NOT = "OK"
                 MOVE "S  A000030." TO VDU
                 MOVE BW-ADR1 TO VDUBUF
                 PERFORM SCREENIO
                 GO TO ENTER-ELE.
           MOVE VDUBUF TO BW-ADR1.
           GO TO CREATE-ROUTINE.
       F-04.
           MOVE VDUBUF TO BW-ADR2.
           GO TO CREATE-ROUTINE.
       F-05.
           IF VDUBUF = SPACES
              IF BW-ADR1 = SPACES AND BW-ADR2 = SPACES
                 MOVE VDUBUF TO BW-CITY
                 GO TO CREATE-ROUTINE
              ELSE
                 MOVE NO-SPACES-SEQ TO PASSWDW-SEQ
                 MOVE "TO REMOVE CITY....:" TO PASSWDW-PROMPT2
                 PERFORM VERIFY-PASSWORD
                 IF PASSWDW-STAT NOT = "OK"
                    MOVE "S  A000050." TO VDU
                    MOVE BW-CITY TO VDUBUF
                    PERFORM SCREENIO
                    GO TO ENTER-ELE.
           MOVE VDUBUF TO BW-CITY.
           GO TO CREATE-ROUTINE.
       F-06.
           IF VDUBUF = SPACES
              IF BW-ADR1 = " " AND BW-ADR2 = " "
                 IF BR-FORCE-VALID-CITY-COUNTY = "Y"
                    MOVE "MUST ENTER A VALID STATE" TO MESS
                    PERFORM SEND-MESS
                    GO TO ENTER-ELE
                 ELSE
                    MOVE VDUBUF TO BW-STATE
                    GO TO F-061
              ELSE
                 MOVE NO-SPACES-SEQ TO PASSWDW-SEQ
                 MOVE "TO REMOVE STATE....:" TO PASSWDW-PROMPT2
                 PERFORM VERIFY-PASSWORD
                 IF PASSWDW-STAT NOT = "OK"
                    MOVE "S  A000060." TO VDU
                    MOVE BW-STATE TO VDUBUF
                    PERFORM SCREENIO
                    GO TO ENTER-ELE
                 ELSE
                    MOVE VDUBUF TO BW-STATE
                    GO TO F-061.

           PERFORM OPEN-ST1-FILE.
           MOVE VDUBUF TO ST-CODE
                          BW-STATE.
           PERFORM READ-ST1-FILE.
           IF IO-BAD
              GO TO ENTER-ELE.

           PERFORM CLOSE-ST1-FILE.

           PERFORM SET-CY-CT-SCAN.

       F-061.
           MOVE "E  A100061." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO F-061.
           IF VDUBUF = SPACES
              MOVE NO-SPACES-SEQ TO PASSWDW-SEQ
              MOVE "TO REMOVE ZIPCODE....:" TO PASSWDW-PROMPT2
              PERFORM VERIFY-PASSWORD
              IF PASSWDW-STAT NOT = "OK"
                 MOVE "S  A000061." TO VDU
                 MOVE BW-ZIP TO VDUBUF
                 PERFORM SCREENIO
                 GO TO F-061.
           MOVE VDUBUF TO BW-ZIP.
           PERFORM VERIFY-ZIPCODE.
           IF ZIPCODE-FLAG = "N"
              MOVE "INVALID FORMAT; MUST BE 12345 OR 12345-1234" TO MESS
              PERFORM SEND-MESS
              GO TO F-061.
           IF GP-USE-BW-BAD-FLAGS NOT = "Y"
              GO TO CREATE-ROUTINE.
       F-062.
           MOVE "E  A010062." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO F-061.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO F-062.
           MOVE VDUBUF TO BW-BAD-ADR-FG.
           IF BW-BAD-ADR-FG NOT = "Y" AND NOT = "N"
              GO TO F-062.

           GO TO CREATE-ROUTINE.
       F-07.
           PERFORM ENTER-PHONE.
           IF TEL-BUFX = " "
               MOVE 0 TO TEL-BUF.
           IF TEL-BUFX IS NOT NUMERIC
               GO TO ENTER-ELE.
           MOVE TEL-BUF TO BW-PHONE.
           PERFORM SEND-PHONE.
           MOVE "S  A120070." TO VDU.
           MOVE "-" TO TEL-H1 TEL-H2.
           MOVE TEL-EDIT TO VDUBUF.
           PERFORM SCREENIO.
           IF GP-USE-BW-BAD-FLAGS NOT = "Y"
              GO TO CREATE-ROUTINE.
           MOVE BW-PHONE TO TEST-PHONE.
           IF BAD-PHONE
              MOVE "S  A010071." TO VDU
              MOVE "Y" TO BW-BAD-PHONE-FG VDUBUF
              PERFORM SCREENIO.
       F-071.
           MOVE "E  A010071." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO F-071.
           MOVE VDUBUF TO BW-BAD-PHONE-FG.
           IF BW-BAD-PHONE-FG NOT = "Y" AND NOT = "N"
              GO TO F-071.
           GO TO CREATE-ROUTINE.

       F-08.
           PERFORM ENTER-PHONE.
           IF TEL-BUFX = " "
               MOVE 0 TO TEL-BUF.
           IF TEL-BUFX IS NOT NUMERIC
               GO TO ENTER-ELE.
           MOVE TEL-BUF TO BW-CELL-PHONE.
           PERFORM SEND-PHONE.
           MOVE "S  A120080." TO VDU.
           MOVE "-" TO TEL-H1 TEL-H2.
           MOVE TEL-EDIT TO VDUBUF.
           PERFORM SCREENIO.

      * TEST IF THE ORIGINAL OPT OUT FIELD IS "M" OR "B" OR "A", AND
      * IF THEY CHANGED THE CELL PHONE, THEN FORCE THE OPT OUT TO "N"
      * AND CREATE A MEMO
           IF ORG-BW-PRIVACY-OPT-OUT = "M" OR "B" OR "A"
              IF BW-CELL-PHONE NOT = ORG-BW-CELL-PHONE
                 MOVE "S  A010380." TO VDU
                 MOVE "N" TO BW-PRIVACY-OPT-OUT VDUBUF
                 PERFORM SCREENIO.

           IF GP-USE-BW-BAD-FLAGS NOT = "Y"
              GO TO CREATE-ROUTINE.

           MOVE BW-CELL-PHONE TO TEST-PHONE.
           IF BAD-PHONE
              MOVE "S  A010081." TO VDU
              MOVE "Y" TO BW-BAD-CELL-FG VDUBUF
              PERFORM SCREENIO.
       F-081.
           MOVE "E  A010081." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO F-081.
           MOVE VDUBUF TO BW-BAD-CELL-FG.
           IF BW-BAD-CELL-FG NOT = "Y" AND NOT = "N"
              GO TO F-081.
           GO TO CREATE-ROUTINE.
       F-09.
           IF VDUNUM0 = 0
              MOVE "S  A080090." TO VDU
              MOVE HOLD-SYS-DATE TO BW-REFDATE DATE-YYYYMMDD
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
              MOVE DATE-MMDDYY TO VDU-DATE-EDIT
              PERFORM SCREENIO
           ELSE
              MOVE VDU-DATE-YYYYMMDD TO BW-REFDATE.
           GO TO CREATE-ROUTINE.

       F-10.
           MOVE VDU-DATE-YYYYMMDD TO BW-BDATE.

           IF BW-BDATE = HOLD-SYS-DATE
              MOVE "ENTER BIRTHDATE, NOT TODAYS DATE" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.

           IF BW-BDATE = 0
              MOVE "MUST ENTER A BIRTHDATE" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.

           MOVE BW-BDATE TO NUM-DATE.
           PERFORM TSTDAT.
           IF NUM-DATE = 0
              MOVE "INVALID DATE" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE
           ELSE
              MOVE BW-BDATE      TO BAGE-DATE
              MOVE HOLD-SYS-DATE TO BAGE-TODAY
              MOVE BW-REFDATE    TO BAGE-REFDATE
              PERFORM BAGE-CALCULATION
              IF BAGE-AGE < 18
                 MOVE AGE-MESSAGE TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-ELE.

           GO TO CREATE-ROUTINE.

       F-11.
           MOVE VDUBUF TO BW-ORIGIN CD-CODE.
           IF BW-ORIGIN NOT = SPACES
              MOVE "OR" TO CD-TYPE
              PERFORM GET-CD
              IF HOLD-IO-FG = 9
                 MOVE "INVALID CODE; F6-SCAN CODES" TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       F-12.
           MOVE VDUNUM0 TO BW-DEPEND.
           GO TO CREATE-ROUTINE.
       F-13.
           MOVE VDUBUF TO BW-RENTCD CD-CODE.
           IF GP-BW-HOUSING-CODE NOT = "N"
              MOVE "BW" TO CD-TYPE
              PERFORM GET-CD
              IF HOLD-IO-FG = 9
                 MOVE "INVALID CODE; F6-SCAN CODES" TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-ELE.

           PERFORM TEST-RENTCD.
           IF IO-FG = 9
              GO TO ENTER-ELE.

           GO TO CREATE-ROUTINE.
       F-14.
           MOVE VDUBUF TO BW-TIMEADDR.

       F-141.
           MOVE "E  A020141." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO F-141.
           MOVE VDUBUF TO BW-DWELLCD.
           GO TO CREATE-ROUTINE.

       F-15.
           MOVE VDUNUM2 TO BW-CREDIT-LIMIT.
           GO TO CREATE-ROUTINE.
       F-16.
           MOVE VDUBUF TO BW-CREDITCD.

       F-16A.
      * FORCE PASSWORD ON MODIFY OF EXISTING RECORD ONLY
            IF NEW-FLAG NOT = 0
              GO TO CREATE-ROUTINE.

           IF NEW-FLAG = 0
              MOVE "TO CHANGE CREDIT SCORE" TO PASSWDW-PROMPT2
              MOVE CRSCORE-SEQ TO PASSWDW-SEQ
              PERFORM VERIFY-PASSWORD
              IF PASSWDW-STAT NOT = "OK"
                 GO TO CREATE-ROUTINE.

       F-16A-BYPASS-PASSWORD.
           MOVE "ENNN040161." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO F-16A-BYPASS-PASSWORD.
           MOVE VDUNUM0 TO BW-CREDIT-SCORE.
           IF GP-BW-CREDIT-SCORE = "Y"
              IF BW-CREDIT-SCORE NOT > 0
                 MOVE "MUST ENTER CREDIT SCORE" TO MESS
                 PERFORM SEND-MESS
                 GO TO F-16A-BYPASS-PASSWORD.
           GO TO CREATE-ROUTINE.
       F-17.
           MOVE VDUBUF TO BW-STRTGCD.
           GO TO CREATE-ROUTINE.
       F-18.
           MOVE VDUBUF TO BW-CENSUSCD.
           GO TO CREATE-ROUTINE.
       F-19.
           MOVE VDUNUM0 TO BW-ORGBR.
           IF BW-ORGBR = 0
              GO TO ENTER-ELE.

           IF ( BW-ORGBR NOT = EXT-CONAME-USER-LOC )
              MOVE SPACES TO MESS
              STRING "WARNING: BRANCH NOT EQUAL USERID [",
                     EXT-CONAME-USER-LOC, "]"
                     DELIMITED BY "  " INTO MESS
              PERFORM SEND-MESS.
           GO TO CREATE-ROUTINE.
       F-20.
           MOVE VDUBUF TO BW-COUNTY.
           IF BR-FORCE-VALID-CITY-COUNTY NOT = "Y"
              GO TO CREATE-ROUTINE.

      *THIS FLAG IS SET TO "Y" IF AN ENTRY FOR THE STATE EXISTS IN THE
      *CODE FILE SO ANY ENTRY FOR THAT STATE MUST BE VERIFIED
           IF  ALLOW-COUNTY-SCAN-FG NOT = "Y"
              GO TO CREATE-ROUTINE.

           MOVE BW-COUNTY TO CD-CT-COUNTY.
           PERFORM DO-STATE-CT-EXIST.

           IF DO-STATE-CT-EXIST-FG NOT = "Y"  OR
               CD-CT-COUNTY NOT = BW-COUNTY
              MOVE "COUNTY NOT ON FILE FOR STATE" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.

           GO TO CREATE-ROUTINE.
       F-21.
           MOVE VDUBUF TO BW-CITY-TAX-CD.

           IF BR-FORCE-VALID-CITY-COUNTY NOT = "Y"
              GO TO CREATE-ROUTINE.

           IF ALLOW-CITY-SCAN-FG NOT = "Y"
              GO TO CREATE-ROUTINE.

           MOVE BW-CITY-TAX-CD TO CD-CY-CITY.
           PERFORM DO-STATE-CY-EXIST.

           IF DO-STATE-CY-EXIST-FG NOT = "Y" OR
              CD-CY-CITY NOT = BW-CITY-TAX-CD
              MOVE "CITY NOT ON FILE FOR STATE" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.

      *ADD A WARNING IF CITY IS ENTERED AS A SPACE
           IF BW-CITY-TAX-CD = SPACES
              MOVE
             "WARNING: YOU HAVE ENTERED SPACE AS THE CITY CODE" TO MESS
              PERFORM SEND-MESS.

            GO TO CREATE-ROUTINE.

       F-22.
            MOVE VDUNUM0 TO BW-ORGSLSMN.
            GO TO CREATE-ROUTINE.

       F-23.
           MOVE VDUBUF TO BW-REPCODE.
           GO TO CREATE-ROUTINE.
       F-24.
           MOVE VDUBUF TO BW-SOLICIT.
           IF NOT (VDUBUF = "Y" OR "N")
              MOVE "Y OR N" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       F-25.
           MOVE VDUBUF TO BW-CONFIDENTIAL.
           IF NOT (VDUBUF = "Y" OR "N")
              MOVE "Y OR N" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       F-26.
           MOVE VDUBUF TO BW-EMPLOYER.
           GO TO CREATE-ROUTINE.
       F-27.
           MOVE VDUBUF TO BW-EMPADDR1.
           GO TO CREATE-ROUTINE.
       F-28.
           MOVE VDUBUF TO BW-EMPADDR2.
           GO TO CREATE-ROUTINE.
       F-29.
           MOVE VDUBUF TO BW-EMPLPOS.
           GO TO CREATE-ROUTINE.
       F-30.
           PERFORM ENTER-PHONE.
           IF TEL-BUFX = " "
              MOVE 0 TO TEL-BUF.
           IF TEL-BUFX NOT NUMERIC
              GO TO ENTER-ELE.
           MOVE TEL-BUF TO BW-EMPPHONE.
           PERFORM SEND-PHONE.
           MOVE "S  A120300." TO VDU.
           MOVE "-" TO TEL-H1 TEL-H2.
           MOVE TEL-EDIT TO VDUBUF.
           PERFORM SCREENIO.
       F-30A.
           MOVE "ENNN040301." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO F-30A.
           MOVE VDUNUM0 TO BW-EMPEXT.
           IF GP-USE-BW-BAD-FLAGS NOT = "Y"
              GO TO CREATE-ROUTINE.
           MOVE BW-EMPPHONE TO TEST-PHONE.
           IF BAD-PHONE
              MOVE "S  A010302." TO VDU
              MOVE "Y" TO BW-BAD-EMPPHONE-FG VDUBUF
              PERFORM SCREENIO.
       F-302.
           MOVE "E  A010302." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO F-30A.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO F-302.
           MOVE VDUBUF TO BW-BAD-EMPPHONE-FG.
           IF BW-BAD-EMPPHONE-FG NOT = "Y" AND NOT = "N"
              GO TO F-302.
           GO TO CREATE-ROUTINE.
       F-31.
           MOVE VDUNUM0 TO BW-EMPYEARS.
       F-311.
           MOVE "E  A090311." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO F-311.

           MOVE VDUBUF TO BW-WORKHRS.
           GO TO CREATE-ROUTINE.
       F-32.
           MOVE VDUNUM2 TO BW-EMPPAY.
           GO TO CREATE-ROUTINE.
       F-33.
           MOVE VDUNUM2 TO BW-EMPPAYOT.
       F-33A.
           MOVE "E  A080331." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO F-33A.
           MOVE VDUBUF TO BW-EMPPAYSR.
           GO TO CREATE-ROUTINE.
       F-34.
           MOVE VDUBUF TO BW-SPLNAME.
           IF GP-EDIT-BORROWER-NAME-FG = "Y"
            IF BW-SPLNAME NOT = SPACES
              MOVE BW-SPLNAME TO WORK-NAME
              PERFORM CHECK-BORROWER-NAME
              IF BAD-NAME-FG = "Y"
                 MOVE
                 "VALID CHARACTERS ARE A-Z, 0-9, SPACE, -, ', AND &"
                    TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       F-35.
           MOVE VDUBUF TO BW-SPFNAME.
           IF GP-EDIT-BORROWER-NAME-FG = "Y"
            IF BW-SPFNAME NOT = SPACES
              MOVE BW-SPFNAME TO WORK-NAME
              PERFORM CHECK-BORROWER-NAME
              IF BAD-NAME-FG = "Y"
                 MOVE
                 "VALID CHARACTERS ARE A-Z, 0-9, SPACE, -, ', AND &"
                    TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       F-36.
           IF VDUNUM0 = 999999999
              GO TO ENTER-ELE.

           IF VDUNUM0 = HOLD-KEY
              MOVE "CAN NOT HAVE SAME SSNO AS CURRENT" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
      * CHOSE TO DO THIS TEST HERE SO NO FLAGS ARE SET YET
           IF VDUNUM0 NOT = 0
              IF BW-SPFNAME = SPACES OR BW-SPLNAME = SPACES
                 MOVE "MUST HAVE SPOUSE NAME TO ENTER A SSNO" TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-ELE.

           MOVE "N" TO SPOUSE-EXISTS.
           PERFORM OPEN-BW1-FILE.
           MOVE BW-REC TO CHANGED-BW-REC.
           MOVE VDUNUM0 TO BW-SPSSNO NUM-BW.
           IF NUM-BW = 0
              GO TO F-36-CONT.

           MOVE NUM-BW TO BW-SSNO.
           PERFORM READ-BW1-FILE.
           IF IO-FG = 0
              MOVE "Y" TO SPOUSE-EXISTS
              MOVE "SSNO ALREADY EXISTS UNDER THE NAME OF" TO MESS
              PERFORM SEND-MESS
              STRING BW-FNAME," ",BW-LNAME,
                                       DELIMITED BY "   " INTO MESS
              PERFORM SEND-MESS.

       F-36-CONT.
           PERFORM CLOSE-BW1-FILE.
           MOVE CHANGED-BW-REC TO BW-REC.

           MOVE NUM-BW TO BW-SPSSNO.
           PERFORM NUMBW-TO-ALPBW.
           MOVE "S  A110360." TO VDU.
           MOVE ALP-BW TO VDUBUF.
           PERFORM SCREENIO.
           GO TO CREATE-ROUTINE.
       F-37.
           MOVE VDU-DATE-YYYYMMDD TO BW-SP-BDATE.

           IF BW-SP-BDATE = HOLD-SYS-DATE
              MOVE "ENTER BIRTHDATE, NOT TODAYS DATE" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.

           IF BW-SP-BDATE NOT = 0
              MOVE BW-SP-BDATE TO NUM-DATE
              PERFORM TSTDAT
              IF NUM-DATE = 0
                 MOVE "INVALID DATE" TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-ELE
              ELSE
                 MOVE BW-SP-BDATE   TO BAGE-DATE
                 MOVE HOLD-SYS-DATE TO BAGE-TODAY
                 MOVE BW-REFDATE    TO BAGE-REFDATE
                 PERFORM BAGE-CALCULATION
                 IF BAGE-AGE < 18
                    MOVE AGE-MESSAGE TO MESS
                    PERFORM SEND-MESS
                    GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       F-38.
           MOVE VDUBUF TO BW-PRIVACY-OPT-OUT.

           IF NOT (VDUBUF = "A" OR "M" OR "B" OR "N" OR "S" OR "D")
              MOVE "ENTER: 'A', 'M', 'B', 'N', 'S', OR 'D' ONLY"
                                                           TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.

           GO TO CREATE-ROUTINE.
       F-39.
           MOVE VDUBUF TO BW-CEASE-DESIST-TYPE.
           IF BW-CEASE-DESIST-TYPE NOT = SPACES AND
                NOT BW-IS-CEASE-DESIST
              MOVE "V,W,E,R,C OR SPACE ONLY" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.

       END-ENTRY-MODULE.
           MOVE 0 TO FORM-SEL1.
       EXIT-ENTRY-MODULE.
           EXIT.

      ***************************************
       CALL-BWCRE2 SECTION.
      ***************************************
           MOVE "WI/BWCRE2" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH
                                BW-REC
                                ALLOW-CHANGES.
           CANCEL FORM-PROGX.

      ***************************************
       CALL-BWCRE3 SECTION.
      ***************************************
           MOVE "WI/BWCRE3" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH
                                BW-REC
                                ALLOW-CHANGES
                                CHAN.
           CANCEL FORM-PROGX.

      ***************************************
       CALL-BWCRE4 SECTION.
      ***************************************
           MOVE "WI/BWCRE4" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH
                                BW-REC
                                ALLOW-CHANGES
                                CHAN.
           CANCEL FORM-PROGX.

      ***************************************
       CALL-BWCRE5 SECTION.
      ***************************************
           MOVE "WI/BWCRE5" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH
                                BW-REC
                                ALLOW-CHANGES
                                CHAN.
           CANCEL FORM-PROGX.


      ******************************************************************
       VERIFY-ZIPCODE SECTION.
      ******************************************************************
           MOVE "Y" TO ZIPCODE-FLAG.

           MOVE BW-ZIP TO TEST-ZIPCODE.

      * 12345
           IF TEST-ZIP-DASH = SPACES
              IF TEST-ZIP-5 NOT NUMERIC
                 MOVE "N" TO ZIPCODE-FLAG
                 GO TO EXIT-VERIFY-ZIPCODE
              ELSE
                 IF TEST-ZIP-4 NOT = SPACES
                 MOVE "N" TO ZIPCODE-FLAG
                 GO TO EXIT-VERIFY-ZIPCODE.

      * 12345-1234
           IF TEST-ZIP-DASH NOT = SPACES
              IF TEST-ZIP-5 NOT NUMERIC
                 MOVE "N" TO ZIPCODE-FLAG
                 GO TO EXIT-VERIFY-ZIPCODE
              ELSE
                 IF TEST-ZIP-DASH NOT = "-"
                    MOVE "N" TO ZIPCODE-FLAG
                    GO TO EXIT-VERIFY-ZIPCODE
                 ELSE
                    IF TEST-ZIP-4 NOT NUMERIC
                       MOVE "N" TO ZIPCODE-FLAG
                       GO TO EXIT-VERIFY-ZIPCODE.
       EXIT-VERIFY-ZIPCODE.
           EXIT.

      ****************************
       WRITE-MODULE SECTION.
      ****************************
           PERFORM OPEN-BW1-FILE.
           IF NEW-FLAG NOT = 0
              GO TO WRITE-RECORD.
           MOVE BW-REC TO CHANGED-BW-REC.
           IF ORG-BW-REC = CHANGED-BW-REC
              GO TO END-WRITE-MODULE.

           PERFORM READ-BW1-FILE.
           IF IO-FG NOT = 0
              PERFORM UNDERGONE-CHANGE
              PERFORM RECORD-DELETED
              GO TO END-WRITE-MODULE.
           IF BW-REC NOT = ORG-BW-REC
              PERFORM UNDERGONE-CHANGE
              PERFORM CHANGE-AGAIN
              GO TO END-WRITE-MODULE.

           MOVE CHANGED-BW-REC TO BW-REC.

      * REGACC PR# 319
           IF ORG-BW-FNAME NOT = BW-FNAME OR
               ORG-BW-LNAME NOT = BW-LNAME OR
                ORG-BW-ADR1 NOT = BW-ADR1 OR
                 ORG-BW-ADR2 NOT = BW-ADR2 OR
                  ORG-BW-CITY NOT = BW-CITY OR
                   ORG-BW-STATE NOT = BW-STATE OR
                    ORG-BW-ZIP NOT = BW-ZIP OR
                     ORG-BW-PHONE NOT = BW-PHONE OR
                      ORG-BW-EMPPHONE NOT = BW-EMPPHONE
              MOVE HOLD-SYS-DATE TO BW-DEALTRACK-CHG-DATE.

           IF GP-USE-BW-BAD-FLAGS = "Y"
              MOVE BW-PHONE TO TEST-PHONE
              IF BAD-PHONE AND BW-BAD-PHONE-FG NOT = "C"
                 MOVE "Y" TO BW-BAD-PHONE-FG
              END-IF
              MOVE BW-EMPPHONE TO TEST-PHONE
              IF BAD-PHONE AND BW-BAD-EMPPHONE-FG NOT = "C"
                 MOVE "Y" TO BW-BAD-EMPPHONE-FG.

           PERFORM REWRITE-BW1-FILE.

           IF ORG-BW-LNAME NOT = BW-LNAME OR
               ORG-BW-FNAME NOT = BW-FNAME
              MOVE "F" TO BI-STAT
              PERFORM UPDATE-BI-FILE.

           MOVE "CL/CHBMMO" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH
                                BW-REC
                                ORG-BW-REC
                                BRANCH.
           CANCEL FORM-PROGX.

           GO TO END-WRITE-MODULE.

       WRITE-RECORD.
           IF BW-SPSSNO = 0
                  OR SPOUSE-FLAG = "Y"
                   OR SPOUSE-EXISTS = "Y"
              MOVE "N" TO DEFAULT-SPOUSE
              MOVE "N" TO SPOUSE-EXISTS
              MOVE "N" TO SPOUSE-FLAG
              GO TO END-DEFAULT-BW1.
           MOVE BW-SSNO TO SAVE-BWSSNO.
           MOVE "Y" TO DEFAULT-SPOUSE.
           PERFORM SAVE-BW-FIELDS.
           GO TO END-DEFAULT-BW1.

       SAVE-BW-FIELDS.
           MOVE "Y" TO SPOUSE-FLAG.
           MOVE BW-SPSSNO TO SAVE-SPSSNO.
           MOVE BW-SPLNAME TO SAVE-SPLNAME.
           MOVE BW-SPFNAME TO SAVE-SPFNAME.
           MOVE BW-SP-BDATE TO HOLD-SP-BDATE DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYYYY.
           MOVE DATE-MMDDYYYY TO SAVE-SP-BDATE.
           MOVE BW-ADR1 TO SAVE-ADDR1.
           MOVE BW-ADR2 TO SAVE-ADDR2.
           MOVE BW-CITY TO SAVE-CITY.
           MOVE BW-STATE TO SAVE-STATE.
           MOVE BW-ZIP TO SAVE-ZIP.
           MOVE BW-PHONE TO TEL-BUF.
           PERFORM SEND-PHONE.
           MOVE TEL-EDIT TO SAVE-PHONE.
           MOVE BW-REFDATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO SAVE-REFDATE.
           MOVE BW-LNAME TO SAVE-LNAME.
           MOVE BW-FNAME TO SAVE-FNAME.
           MOVE BW-SSNO TO NUM-BW.
           PERFORM NUMBW-TO-ALPBW.
           MOVE ALP-BW TO SAVE-SSNO.
           MOVE BW-BDATE TO HOLD-BDATE DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYYYY.
           MOVE DATE-MMDDYYYY TO SAVE-BDATE.

       END-DEFAULT-BW1.
           IF GP-USE-BW-BAD-FLAGS = "Y"
              MOVE BW-PHONE TO TEST-PHONE
              IF BAD-PHONE AND BW-BAD-PHONE-FG NOT = "C"
                 MOVE "Y" TO BW-BAD-PHONE-FG
              END-IF
              MOVE BW-EMPPHONE TO TEST-PHONE
              IF BAD-PHONE AND BW-BAD-EMPPHONE-FG NOT = "C"
                 MOVE "Y" TO BW-BAD-EMPPHONE-FG.

           PERFORM WRITE-BW1-FILE.
           IF IO-FG NOT = 0
              PERFORM ALREADY-CREATED.

       END-WRITE-MODULE.
           PERFORM CLOSE-BW1-FILE.

      *****************************
       DELETE-MODULE SECTION.
      *****************************
           IF NEW-FLAG NOT = 0
              GO TO END-DELETE-MODULE.

           PERFORM OPEN-BW1-FILE.
           PERFORM READ-BW1-FILE.
           IF IO-FG NOT = 0
              PERFORM UNDERGONE-CHANGE
              PERFORM RECORD-DELETED
              GO TO END-DELETE-MODULE.
           IF BW-REC NOT = ORG-BW-REC
              PERFORM UNDERGONE-CHANGE
              PERFORM RECORD-CHANGED
              GO TO END-DELETE-MODULE.
           PERFORM DELETE-BW1-FILE.

           PERFORM OPEN-BX1-FILE.
           MOVE HOLD-KEY TO BX-SSNO.
           PERFORM READ-BX1-FILE.
           IF IO-FG = 0
              PERFORM DELETE-BX1-FILE.
           PERFORM CLOSE-BX1-FILE.

       END-DELETE-MODULE.
           PERFORM CLOSE-BW1-FILE.

      ***********************************
       VERIFY-DELETE SECTION.
      ***********************************
           MOVE 9 TO IO-FG.
           MOVE "CL/BWVDEL" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH
                                BW-REC
                                IO-FG.
           CANCEL FORM-PROGX.
           IF IO-FG = 9
              MOVE "ACTIVE BORROWER, DELETE IGNORED !" TO MESS
              PERFORM SEND-MESS.

      ***********************************
       DISPLAY-FIELDS SECTION.
      ***********************************
           PERFORM SET-CY-CT-SCAN.
           MOVE " " TO DISPLAY-BUF.
           MOVE BW-LNAME TO F01.
           MOVE BW-FNAME TO F02.
           MOVE BW-ADR1 TO F03.
           MOVE BW-ADR2 TO F04.
           MOVE BW-CITY TO F05.
           MOVE BW-STATE TO F06.
           MOVE BW-ZIP TO F061.
           MOVE BW-BAD-ADR-FG TO F062.
           IF BW-PHONE NOT = 0
              MOVE BW-PHONE TO TEL-BUF
              PERFORM SEND-PHONE
              MOVE TEL-EDIT TO F07.
           MOVE BW-BAD-PHONE-FG TO F071.
           IF BW-CELL-PHONE NOT = 0 AND
                             NOT = 0202020202 AND
                              NOT = 2020202020
              MOVE BW-CELL-PHONE TO TEL-BUF
              PERFORM SEND-PHONE
              MOVE TEL-EDIT TO F08.
           MOVE BW-BAD-CELL-FG TO F081.
           MOVE BW-REFDATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO F09.
           MOVE BW-BDATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYYYY.
           MOVE DATE-MMDDYYYY TO F10.
           MOVE BW-ORIGIN TO F11.
           MOVE BW-DEPEND TO F12.
           MOVE BW-RENTCD TO F13.
           MOVE BW-TIMEADDR TO F14.
           MOVE BW-DWELLCD TO F141.
           MOVE BW-CREDIT-LIMIT TO F15.
           MOVE BW-CREDITCD TO F16.
           MOVE BW-CREDIT-SCORE TO F161.
           MOVE BW-STRTGCD TO F17.
           MOVE BW-CENSUSCD TO F18.
           MOVE BW-ORGBR TO F19.
           MOVE BW-COUNTY TO F20.
           MOVE BW-CITY-TAX-CD TO F21.
           MOVE BW-ORGSLSMN TO F22.
           MOVE BW-REPCODE TO F23.
           MOVE BW-SOLICIT TO F24.
           MOVE BW-CONFIDENTIAL TO F25.
           MOVE BW-EMPLOYER TO F26.
           MOVE BW-EMPADDR1 TO F27.
           MOVE BW-EMPADDR2 TO F28.
           MOVE BW-EMPLPOS TO F29.
           IF BW-EMPPHONE NOT = 0
              MOVE BW-EMPPHONE TO TEL-BUF
              PERFORM SEND-PHONE
              MOVE TEL-EDIT TO F30.
           MOVE BW-EMPEXT TO F301.
           MOVE BW-BAD-EMPPHONE-FG TO F302.
           MOVE BW-EMPYEARS TO F31.
           MOVE BW-WORKHRS TO F311.
           MOVE BW-EMPPAY TO F32.
           MOVE BW-EMPPAYOT TO F33.
           MOVE BW-EMPPAYSR TO F331.
           MOVE BW-SPLNAME TO F34.
           MOVE BW-SPFNAME TO F35.
           IF BW-SPSSNO > ZERO
               MOVE BW-SPSSNO TO F36
               INSPECT F36 REPLACING ALL "/" BY "-".
           MOVE BW-SP-BDATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYYYY.
           MOVE DATE-MMDDYYYY TO F37.
           MOVE BW-PRIVACY-OPT-OUT TO F38.
           MOVE BW-CEASE-DESIST-TYPE TO F39.

           MOVE SPACES TO F051 F052.
           PERFORM OPEN-BX1-FILE.
           MOVE BW-SSNO TO BX-SSNO.
           PERFORM READ-BX1-FILE.
           IF IO-FG = 0
              IF BX-ALT-ADDRESS NOT = SPACES
                MOVE "ALT:" TO F051
                MOVE "Y" TO F052.
           PERFORM CLOSE-BX1-FILE.

           MOVE DISPLAY-BUF TO WR-BUF.
           MOVE DISPLAY-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      ********************************
       CEASE-POP-UP SECTION.
      ********************************
           MOVE 6 TO WINDOW-LINES.
           MOVE 45 TO WINDOW-COLUMNS.
           MOVE 5 TO WINDOW-BEGIN-Y.
           MOVE 5 TO WINDOW-BEGIN-X.
           MOVE "CEASED" TO VDU-FORM-NAME.
           MOVE "WI/CEASED" TO WINDOW-FORM-NAM.

           PERFORM OPEN-WINDOW.
           DISPLAY CEASED-SCREEN.

           IF BW-CEASE-DESIST-TYPE = "C"
              MOVE "COMBINATION" TO MESS
           ELSE
           IF BW-CEASE-DESIST-TYPE = "W"
              MOVE "WRITTEN" TO MESS
           ELSE
           IF BW-CEASE-DESIST-TYPE = "V"
              MOVE "VERBAL" TO MESS
           ELSE
           IF BW-CEASE-DESIST-TYPE = "E"
              MOVE "EMPLOYER" TO MESS
           ELSE
           IF BW-CEASE-DESIST-TYPE = "R"
              MOVE "REFERENCE" TO MESS.
           MOVE "S  A011C-MESS." TO VDU.
           MOVE MESS   TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "PRESS <RETURN> TO CONTINUE" TO WR-BUF.
           MOVE "WSEL."                      TO WR-LIST.
           PERFORM CALL-WRFORM.
           MOVE "E  A010WSELX." TO VDU.
           PERFORM SCREENIO.

           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY CEASED-SCREEN.
           PERFORM CLOSE-WINDOW.
           MOVE "MODBOR" TO VDU-FORM-NAME.

      *****************************************************************
       CODE-SCAN SECTION.
      *****************************************************************

           MOVE SPACES TO CDSCAN-CODE.

           MOVE "WI/CDSCAN" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH CDSCAN-BUF EXIT-PATHNAME.
           CANCEL FORM-PROGX.
           IF CDSCAN-STATUS NOT = "00" OR CDSCAN-CODE = SPACES
              MOVE 9 TO IO-FG
              GO TO EXIT-CODE-SCAN.

           MOVE "S  A040   ." TO VDU.
           MOVE ELE TO VDUROW.
           MOVE 0    TO VDUCOL.
           MOVE CDSCAN-CODE TO VDUBUF.
           PERFORM SCREENIO.

           MOVE CDSCAN-CODE TO VDUBUF.
           MOVE 0 TO IO-FG.

       EXIT-CODE-SCAN.
           EXIT.


      *************************************************
       CYSCAN-SCAN SECTION.
      *************************************************
           MOVE SPACES      TO CYSCAN-CD1-KEY.
           MOVE "CY"        TO CYSCAN-KEY-TYPE.
           MOVE BW-STATE    TO CYSCAN-KEY-STATE.
           MOVE ST-DESC     TO CYSCAN-ST-DESC.
           MOVE "WI/CYSCAN " TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH CYSCAN-BUF EXIT-PATHNAME.
           CANCEL FORM-PROGX.
           IF (CYSCAN-STATUS NOT = "00") OR (CYSCAN-KEY-CITY = SPACES)
              MOVE 9 TO IO-FG
              GO TO CYSCAN-SCAN-EXIT.

           MOVE "S  A030210." TO VDU.
           MOVE CYSCAN-KEY-CITY TO VDUBUF.
           PERFORM SCREENIO.

           MOVE 0 TO IO-FG.
           MOVE CYSCAN-KEY-CITY TO VDUBUF.

       CYSCAN-SCAN-EXIT.
           EXIT.

      *************************************************
       CTSCAN-SCAN SECTION.
      *************************************************
           MOVE SPACES      TO CTSCAN-CD1-KEY.
           MOVE "CT"        TO CTSCAN-KEY-TYPE.
           MOVE BW-STATE    TO CTSCAN-KEY-STATE.
           MOVE ST-DESC     TO CTSCAN-ST-DESC.

           MOVE "WI/CTSCAN " TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH CTSCAN-BUF EXIT-PATHNAME.
           CANCEL FORM-PROGX.

           IF (CTSCAN-STATUS NOT = "00") OR (CTSCAN-KEY-COUNTY = SPACES)
              MOVE 9 TO IO-FG
              GO TO CTSCAN-SCAN-EXIT.

           MOVE "S  A030200." TO VDU.
           MOVE CTSCAN-KEY-COUNTY TO VDUBUF.
           PERFORM SCREENIO.

           MOVE 0 TO IO-FG.
           MOVE CTSCAN-KEY-COUNTY TO VDUBUF.

       CTSCAN-SCAN-EXIT.
           EXIT.


      *******************************************************
       VERIFY-PASSWORD SECTION.
      *******************************************************
           MOVE " " TO PASSWDW-STAT.
           MOVE "MODBOR" TO PASSWDW-PGM.
           MOVE "PLEASE ENTER THE PASSWORD," TO PASSWDW-PROMPT1.
           MOVE "CL/PASSWD" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH PASSWDW-BUF.
           CANCEL FORM-PROGX.

      **************************************
       VERIFY-PASSWORD-MUST-EXIST SECTION.
      **************************************
           MOVE "EN"                         TO PASSWDW-STAT.
           MOVE "CHBORR"                     TO PASSWDW-PGM.
           MOVE "PLEASE ENTER THE PASSWORD," TO PASSWDW-PROMPT1.
           MOVE "CL/PASSWD" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH PASSWDW-BUF.
           CANCEL FORM-PROGX.

      ******************************************************
       SET-CY-CT-SCAN SECTION.
      ******************************************************

      * SET FLAGS TO ALLOW OR DISALLOW SCAN ON CITY & COUNTY
      * DEPENDING IF THE STATE IS IN CODE FILE (TYPE CY & CT)

           IF BR-FORCE-VALID-CITY-COUNTY NOT = "Y"
              MOVE "N" TO ALLOW-CITY-SCAN-FG
           ELSE
              MOVE " " TO CD-CY-CITY
              PERFORM DO-STATE-CY-EXIST
              IF DO-STATE-CY-EXIST-FG = "Y"
                 MOVE "Y" TO ALLOW-CITY-SCAN-FG
              ELSE
                 MOVE "N" TO ALLOW-CITY-SCAN-FG.

           MOVE "N" TO ALLOW-COUNTY-SCAN-FG.
           IF BR-FORCE-VALID-CITY-COUNTY = "Y"
              MOVE " " TO CD-CT-COUNTY
              PERFORM DO-STATE-CT-EXIST
              IF DO-STATE-CT-EXIST-FG = "Y"
                 MOVE "Y" TO ALLOW-COUNTY-SCAN-FG.
       SET-CY-CT-SCAN-EXIT.
           EXIT.

      ******************************************************
       DO-STATE-CY-EXIST SECTION.
      ******************************************************

      * DETERMINE IF A VALID CD-CY-REC EXISTS FOR THE BORROWERS STATE
           MOVE " " TO DO-STATE-CY-EXIST-FG.

           PERFORM OPEN-CD1-FILE.
           MOVE "CY"       TO CD-TYPE.
      * SETUP CD-CY-CITY BEFORE PERFORM DO-STATE-CY-EXIST
           MOVE SPACES     TO CD-CY-FILLER.
           MOVE BW-STATE   TO CD-CY-STATE.

           PERFORM START-CD1-FILE.
           PERFORM READ-CD1-FILE-NEXT.
           IF IO-BAD
              GO TO DO-STATE-CY-EXIST-EXIT.

           IF CD-TYPE = "CY" AND CD-CY-STATE = BW-STATE
              MOVE "Y" TO DO-STATE-CY-EXIST-FG.

       DO-STATE-CY-EXIST-EXIT.
           PERFORM CLOSE-CD1-FILE.

      ******************************************************
       DO-STATE-CT-EXIST SECTION.
      ******************************************************

      * DETERMINE IF A VALID CD-CT-REC EXISTS FOR THE BORROWERS STATE

           MOVE " " TO DO-STATE-CT-EXIST-FG.

           PERFORM OPEN-CD1-FILE.
           MOVE "CT"       TO CD-TYPE.
      * SETUP CD-CT-COUNTY BEFORE PERFORM DO-STATE-CT-EXIST
           MOVE SPACES     TO CD-CT-FILLER.
           MOVE BW-STATE   TO CD-CT-STATE.

           PERFORM START-CD1-FILE.
           PERFORM READ-CD1-FILE-NEXT.
           IF IO-BAD
              GO TO DO-STATE-CT-EXIST-EXIT.

           IF CD-TYPE = "CT" AND CD-CT-STATE = BW-STATE
              MOVE "Y" TO DO-STATE-CT-EXIST-FG.

       DO-STATE-CT-EXIST-EXIT.
           PERFORM CLOSE-CD1-FILE.

      ******************************************************
       VALIDATE-REQUIRED-FIELD SECTION.
      ******************************************************

      * MUST VERIFY IF BR-FORCE-VALID-CITY-COUNTY = "Y", THAT THE
      * FIELD ENTRIES IN CITY & COUNTY EXIST IN THE CODE-FILE FOR
      * TYPES CT & CY

           MOVE "N" TO ALLOW-BW-UPDATE-FG.
           IF BR-FORCE-VALID-CITY-COUNTY NOT = "Y"
              MOVE "Y" TO ALLOW-BW-UPDATE-FG
              GO TO VALIDATE-REQUIRED-FIELD-EXIT.

           IF ALLOW-CITY-SCAN-FG NOT = "Y"
               MOVE "Y" TO ALLOW-BW-UPDATE-FG
               GO TO CHECK-FOR-VALID-COUNTY.

           MOVE BW-CITY-TAX-CD  TO CD-CY-CITY.
           PERFORM DO-STATE-CY-EXIST
           IF DO-STATE-CY-EXIST-FG NOT = "Y" OR
                  CD-CY-CITY NOT = BW-CITY-TAX-CD
              MOVE "N" TO ALLOW-BW-UPDATE-FG
              MOVE "CITY NOT ON FILE FOR STATE" TO MESS
              PERFORM SEND-MESS
              GO TO VALIDATE-REQUIRED-FIELD-EXIT.

       CHECK-FOR-VALID-COUNTY.
           IF ALLOW-COUNTY-SCAN-FG NOT = "Y"
              GO TO VALIDATE-REQUIRED-FIELD-EXIT.

           MOVE BW-COUNTY   TO CD-CT-COUNTY.
           PERFORM  DO-STATE-CT-EXIST.

           IF DO-STATE-CT-EXIST-FG NOT = "Y" OR
               CD-CT-COUNTY NOT = BW-COUNTY
               MOVE "N" TO ALLOW-BW-UPDATE-FG
               MOVE "COUNTY NOT ON FILE FOR STATE" TO MESS
               PERFORM SEND-MESS
               GO TO VALIDATE-REQUIRED-FIELD-EXIT.

       VALIDATE-REQUIRED-FIELD-END.
           MOVE "Y" TO ALLOW-BW-UPDATE-FG.
       VALIDATE-REQUIRED-FIELD-EXIT.
           EXIT.

      ***********************************************
       CHECK-BORROWER-NAME SECTION.
      ***********************************************
           MOVE 1 TO SUB SUB2.
           MOVE "N" TO BAD-NAME-FG.
       CHECK-NAME-LOOP.
           IF SUB > 16
              GO TO EXIT-CHECK-NAME.

           MOVE NAME-CHAR(SUB) TO TEST-CHAR.
           IF TEST-CHAR NOT ALPHABETIC
               IF TEST-CHAR NOT NUMERIC
                  IF NOT VALID-CHAR
                     MOVE "Y" TO BAD-NAME-FG
                     GO TO EXIT-CHECK-NAME.
           ADD 1 TO SUB SUB2.
           GO TO CHECK-NAME-LOOP.
       EXIT-CHECK-NAME.
           EXIT.

       COPY "LIBWI/PFSWINDOWS.CPY".
       COPY "LIBGB/GPENV.CPY".
       COPY "LIBGB/DATER.CPY".
       COPY "LIBLP/LPBWCL.CPY".
       COPY "LIBLP/LPBAGE.CPY".
       COPY "LIBGB/SCREEN.CPY".

      *********************************************
       FORM-RESET SECTION.
      *********************************************
           DISPLAY SPACES UPON MODBOR-WINDOW-HANDLE
                          LINE 1 COL 1 ERASE SCREEN.
           DISPLAY MODBOR-SCREEN UPON MODBOR-WINDOW-HANDLE.
           MOVE MODBOR-FORM-FIELDS TO WR-BUF.
           MOVE MODBOR-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
       FORM-RESET-EXIT.
           EXIT.

      *********************************************
       SETUP-LINK-INFO SECTION.
      *********************************************
           MOVE MODBOR-HELP--FILE TO HELP-LINK-FILE.
           MOVE MODBOR-HELP--DIR TO HELP-LINK-DIR.

      *********************************************
       VDU-CONVERT-FIELD SECTION.
      *********************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBWI/MODBOR_EVA.CPY".
              COPY "LIBWI/CEASED_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      *************************
       MISC-PARAGRAPHS SECTION.
      *************************
       COPY "LIBGB/PHONEIO.CPY".
       COPY "LIBGB/ACCESS.CPY".
       UPDATE-BI-FILE.
           MOVE 0 TO BI-LN-ACCTNO BI-CHTO-SSNO BI-LN-SSNO(2)
                     BI-LN-SSNO(3) BI-LN-SSNO(4)
                     BI-LN-ENTDATE BI-LN-POFFDATE BI-LN-PLDATE.
           MOVE SPACES TO BI-LN-ORGST BI-LN-SALEFIN BI-LN-GIFTNET
                          BI-LN-PLCD BI-LN-CONVERCD BI-LN-SMALLOAN.
           MOVE BW-SSNO TO BI-LN-SSNO(1).
           MOVE BRANCH TO BI-LN-OWNBR.

           MOVE "CL/BICREA" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH 
                                BI-STAT
                                BI-LN-ACCTNO
                                BI-LN-BWTABLE
                                BI-LN-OWNBR
                                BI-LN-ORGST
                                BI-LN-SALEFIN
                                BI-LN-GIFTNET
                                BI-LN-ENTDATE
                                BI-LN-POFFDATE
                                BI-LN-PLCD
                                BI-LN-CONVERCD
                                BI-CHTO-SSNO
                                BI-LN-SMALLOAN
                                BI-LN-PLDATE.
           CANCEL FORM-PROGX.
       TEST-RENTCD.
           MOVE 9 TO IO-FG.
           IF VDUTAB(1) NOT NUMERIC
              IF VDUTAB(2) NOT NUMERIC
                 IF VDUTAB(3) NOT NUMERIC
                    IF VDUTAB(4) NOT NUMERIC
                       MOVE 0 TO IO-FG.
       RECORD-DELETED.
           MOVE " RECORD WAS DELETED!" TO MESS.
           PERFORM SEND-MESS.
       RECORD-CHANGED.
           MOVE " CHANGES WERE MADE TO THIS RECORD!" TO MESS.
           PERFORM SEND-MESS.
       CHANGE-AGAIN.
           MOVE " PLEASE RE-INPUT YOUR CHANGES!" TO MESS.
           PERFORM SEND-MESS.
       UNDERGONE-CHANGE.
           MOVE
           " THIS RECORD HAS UNDERGONE CHANGE BY ANOTHER USER,"
           TO MESS.
           PERFORM SEND-MESS.
       ALREADY-CREATED.
           MOVE " RECORD HAS BEEN ALREADY CREATED!" TO MESS.
           PERFORM SEND-MESS.
       NUMBW-TO-ALPBW.
           MOVE NUM-BW123 TO ALP-BW123.
           MOVE NUM-BW45 TO ALP-BW45.
           MOVE NUM-BW6789 TO ALP-BW6789.
           MOVE "-" TO ALP-HYPHEN1 ALP-HYPHEN2.
       ALPBW-TO-NUMBW.
           MOVE ALP-BW123 TO NUM-BW123.
           MOVE ALP-BW45 TO NUM-BW45.
           MOVE ALP-BW6789 TO NUM-BW6789.
           IF NUM-BW NOT NUMERIC
              MOVE 0 TO NUM-BW.
       SEND-LEGEND.
           MOVE "S  A700WSEL:D." TO VDU.
           MOVE LEGEND TO VDUBUF.
           PERFORM SCREENIO.
       GET-CD.
           PERFORM OPEN-CD1-FILE.
           PERFORM READ-CD1-FILE.
           MOVE IO-FG TO HOLD-IO-FG.
           PERFORM CLOSE-CD1-FILE.

      **************************
       IO-ROUTINE SECTION.
      **************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-ST1-FILE.
           PERFORM CLOSE-CD1-FILE.
           PERFORM CLOSE-BX1-FILE.
           PERFORM CLOSE-BW4-FILE.
           PERFORM CLOSE-BW3-FILE.
           PERFORM CLOSE-BW2-FILE.
           PERFORM CLOSE-BW1-FILE.
           PERFORM CLOSE-BR-FILE.
       COPY "LIBLP/LPSTGS_SQL.CPY".
       COPY "LIBLP/LPCDGS_SQL.CPY".
       COPY "LIBLP/LPBXGS_SQL.CPY".
       COPY "LIBLP/LPBWGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBLP/LPST1RN.CPY".
       COPY "LIBLP/LPBW1IN.CPY".
       COPY "LIBLP/LPBW2RN.CPY".
       COPY "LIBLP/LPBW3RN.CPY".
       COPY "LIBLP/LPBW4RN.CPY".
       COPY "LIBLP/LPBX1IN.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBLP/LPCD1RN.CPY".

       COPY "LIBGB/FERRORS.CPY".
       END-ROUTINE.
           PERFORM CLOSE-FILES.
       EXIT-PROG.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY MODBOR-SCREEN.
           DESTROY MODBOR-WINDOW-HANDLE.
       STOP-RUN.
           EXIT PROGRAM.
