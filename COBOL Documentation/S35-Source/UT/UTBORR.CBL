      *%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      *%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      *%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      *%%% TEST FOR /USR/LOCAL/BIN/SQL-PGM-COL.SH
      *%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      *%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      *%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
       IDENTIFICATION DIVISION.
       PROGRAM-ID.      CHBORR.
       DATE-WRITTEN.    080492.
      *****************************************************
      *                 (WI)
      *
      *  DESCRIPTION:   CHANGE BORROWER RECORD.
      *                 CANNOT CREATE A NEW BORROWER HERE !!
      *
      *                    CALLED FROM CL/CLMENU.C
      *                      F9 INTERRUPT
      *
      * REV:
      *  MJD 120996 FORCE ENTRY OF CITY/STATE IF ADDRESS 1/2 IS ENTERED.
      *             REMOVED NUMERIC TEST OF ZIP.
      *  BLV 022097 ADD BW-CREDIT-SCORE, FIELD 16-1
      *  KEC 041697 CHANGED ACCESS TO BW-REPCODE FROM NUMERIC TO ALPHA.
      *  MJD 050997 ADDED GP-BW-CREDIT-SCORE TO FORCE ENTRY OF BW-CREDIT-SCORE
      *             REQ#E155.
      *  JTG 082197 ADDED BW-OTHPHONE LOGIC WORLD A90
      *  GLF 090997 REPLACED 'IO' ON GP-FILE WITH GET 'GPENV'
      *  MJD 112497 EXPANDED BW-REC TO 480 BYTES
      *  MJD 021998 ADDED BW-SIZE (78 LEVEL FOR REC SIZE)
      *             MOVED BWCRE2 TO BWCRE3, ADDED NEW BWCRE2
      *  GLF 030298 DELETED BW-OTHPHONE AND ARRANGED FIELDS ACCORDINGLY
      *  MJD 990601 ADDED PASSWORD TO CHANGE OF CREDIT SCORE
      *             (REQ#865 - REGIONAL-ACCEPT.)
      *  GLF 062599 ADD PASSWORD FOR CHANGING REFERENCE DATE
      *  GLF 991709 CHANGE SSNO ENTRY TO USE NEW VDUCLASS "X" EDIT
      *  BAH 000417 ADD PASSWORD FOR CHANGING CONFIDENTIAL CODE, PR# 79 TFC
      *                 WI CHBORR OPT=2
      *  MJD 000503 ADDED CONTACT PHONE #'S FROM BM-REC (TFC PR#37)
      *  BAH 001004 FORCE BW-REPCODE ENTRY TO "YES" OR "NO" FOR TAX PRE
      *             REGISTRATION, WORLD PR# 240
      *  BAH 010228 DO NOT WRITE A BORROWER MEMO FOR ANY CHANGES IF
      *             GP-I-AM-MODERN, MODERN PR# 1437
      *  CS  010626 ADDED CHECK OF CODE FILE FOR VALID HOUSING CODES IF
      *             GP-BW-HOUSING-CODE NOT = "N"  (AMTHRIFT PR#328)
      *  BAH 010627 ADDED BW-PRIVACY-OPT-OUT 37. OPT OUT : FOR TFC , PR# 90
      *             ALSO PASSWORD TURNING IT OFF
      *  BAH 010711 PASSWORD SMSA (BW-STRTGCD) FOR WORLD, PR# 269
      *  CS  010823 DONT ALLOW ENTRY OF BW-PRIVACY-OPT-OUT IF
      *             GP-BW-PRIVACY-OPT-OUT NOT = "Y"  PR#1571 STLOUIS
      *  CS  020219 ADDED "CT" COUNTY TAX CODE KENTUCKY & PERCENTS
      *             ADDED "CY" CITY   TAX CODE KENTUCKY & PERCENTS
      *  CS  020219 IF BR-FORCE-VALID-CITY-COUNTY = "Y", EXAMINE THE CODE
      *             FILE FOR ANY ENTRIES FOR THAT STATE.  IF ENTRIES EXIST FOR
      *             THE STATE, ALL ENTRIES FOR STATE MUST BE VALIDATED.
      *  CS  020304 CORRECTED SET OF CD-BW-CODE (F-13) [CINDY]
      *  CS  020312 REMOVED EXTRA PERIOD FROM SET-CY-CT SECTION
      *  BAH 020808 ADDED PASSWORD ON BW-LNAME, COLLECTORS ARE CHANGING
      *             LAST NAME TO GET THEM OUT OF THEIR QUEUE !! TFC PR# 94
      *  BAH 021007 IF PHONE CONTACTS ARE SPACED OUT (DELETED), THEN DELETE
      *             THE BORROWER MEMO RECORD !! TFC
      *  CS  021203 IF BR-FORCE-VALID-CITY-COUNTY = "Y", ISSUE A WARNING IF
      *             BW-CITY-TAX-CODE =  SPACES   [WORLD PR#288]
      *  BAH 030205 ADDED BW-CALL-ON-SUNDAY FIELD 39.  REGACC PR# 15
      *  BAH 030410 ADDED BW-BAD-ADR-FG AND BW-BAD-PHONE-FG TRIAD REGACC PR# 26
      *  BAH 030430 ADDED SET OF BW-ADR-PHONE-CHG-DATE FOR TRIAD REGACC PR# 26
      *  BAH 030520 ALLOW PAGE 4, BWCRE4, REGACC PR# 27
      *  BAH 030520 ADDED DISPLAY OF "ALT: Y" IF BXFILE HAS AN ALTERNATE
      *            ADDRESS IN IT, REGACC PR# 27
      * BAH 030728 ADDED BW-BAD-EMPPHONE-FG TRIAD REGACC PR# 36
      * BAH 031114 ADDED GP-USE-BW-BAD-FLAGS FOR THE ENTRY OF THE ABOVE REGACC
      *             FIELDS...SUPPORT PR# 2155
      * BAH 040218 AUTOMATICALLY SET BW-BAD-PHONE-FG FOR SPECIFIC
      *            PHONE NUMBERS, REGACC PR# 48
      * BAH 040301 ADDED UPDATE-BI-FILE, BI-STAT = "F" TO UPDATE
      *            ANY CHANGE TO LNAME AND FNAME, PCCREDIT PL# 389
      * BAH 040324 BW-RENTCD WAS GETTING WIPED OUT...MOVE VDUBUF WAS
      *            INSIDE AN 'IF' STATEMENT, REGWAN PL# 435
      * BAH 040409 CHANGE PROMPT 24 SOLICIT TO 24 CONTACT AND PROMPT 39 FROM
      *            CALL SUNDAY TO 39 OK SOLICIT FOR LENDMARK PR# 2271
      *            ALSO ADDED PASSWORDS FOR FIELD 24 AND 38
      * BAH 061116 FORGOT TO DELETE BX TRAILER RECORD, LENDMARK PR# 2044
      * MJD 070130 ADDED NEW BWCRE5 - BORROWER
      *            "KNOW YOUR CUSTOMER" INFO REGACC
      *            ADDED PW PROTECTION FOR REFDATE (NEW RECORD MODIFY)
      *            AND SKIPPED FIELD ON NEW RECORD CREATE IF REGENCY.
      *                                                    REGENCY PR#2856
      * BAH 080401 TEST THAT AGE MUST BE GREATER THAN 18 YRS AND BIRTHDATE
      *            CAN NOT BE TODAYS DATE, WORLD PR# 577
      * BAH 080707 ADDED FIELD 8. BW-CELL-PHONE UNITED PR# 3205
      * BAH 090112 DONT ALLOW SAME SSNO FOR BORROWER AND SPOUSE
      * BAH 091005 PASSWORD CLEARING ADR,CITY,STATE,ZIP FOR REGACC PR# 256
      * BAH 100106 PASSWORD CHANGING LNAME,FNAME,SECOND LNAME,RFC WORLD
      *               MEXICO PR# 667
      * BAH 100216 WAS NOT GOING TO ZIP IF YOU SPACES OUT THE STATE, WORLD
      * BAH 100608 ADDED BW-BAD-CELL-FG FIELD 081, REGACC PR# 266
      *  CS 100915 ADDED PASSWORD SEQ 11 FOR CHANGE REP CODE   WORLD #707
      *  CS 110510 ADD TEST IF GP-I-AM-REGENCY & BR-STATE = "KY" FORCE "KY"
      *            TO CD-CY-STATE & CD-CT-STATE. REGENCY WANTS ALL BORROWERS
      *            TO BE CHARGED THE INSURANCE TAX REGARDLESS OF THE BORROWER
      *            STATE   REGENCY PR#3784
      * BAH 110801 ADDED MXCREA FOR SPANISH POP UP WINDOW, WORLD PR# 754
      * BAH 120417 ADDED SET OF FORM-SPANISH AND MOVED GETFM FOR WORLD PR# 800
      * BAH 120828 PASSWORD CHANGING FIRST NAME FNAME, REGMGM PR# 4100
      * BAH 120830 ADDED SET OF BW-DEALTRACK-CHG-DATE FOR REGACC PR# 319
      * BAH 120927 ADDED BW-WORKHRS-NUM FOR GP-I-AM-MULL, MULLEN PR# 4092
      * BAH 130819 CHANGE 38 OPT OUT FOR WORLD TO TXT OPTION, WORLD PR# 919
      * BAH 140225 CHANGE VALUES ALLOWED FOR WORLD TXT OPTION, WORLD PR# 970
      * BAH 140512 REMOVED FORM-SPANISH ADDED EXT-SPANISH-FORMS
      * BLM 141001 ALLOW VALUE OF "D" (DECLINED) FOR WORLD TXT OPTION 
      *            (BW-PRIVACY-OPT-OUT), WORLD PR# 1023
      * BAH 150728 ADDED PASSWORD FOR FIELDS 34-37 AND 21, SOAUTO PR# 4810
      * BAH 150903 FORCE BW-PRIVACY-OPT-OUT TO "N" IF CELL PHONE CHANGES
      *            AND WRITE A MEMO, WORLD PR# 1101
      * BAH 151030 REDISPLAY BW-REPCODE IF BWCRE2 CHANGED IT, HONOR PR#4898
      * BAH 151216 ALLOW "C" IN BAD PHONE FLAGS, SOAUTO PR#4912
      * BAH 160217 SET BM-BRNO, PD0003
      *
      * KEC 2016.0328 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         CHANGED CDSCAN-WINDOW-BUF TO HAVE MATCHING CD1-KEY
      *         LAYOUTS (FOR EACH CODE TYPE; CD-TYPE).
      *         CDSCAN-CD1-KEY IS THE NEW (PRIMARY) WITH REDEFINES OF
      *         CDSCAN-??-KEY (NEW).
      *
      * KEC 2016.0328 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         CHANGED CYSCAN WORKERS TO MATCH CD1-KEY FOR CY LAYOUT.
      *         CHANGED CTSCAN WORKERS TO MATCH CD1-KEY FOR CT LAYOUT.
      * BAH 160422 ADDED BI-LN-SMALLOAN FOR DUAL TEST, REGENCY PR#5030
      * BAH 160510 CHANGED PASSWORD 13 FOR JUST CITY, SOAUTO PR#5066
      * BAH 160523 ADDED PASSWORD 14 TO CHANGE BIRTHDATE, SOAUTO PR#5080
      * BAH 160822 MOVED BM-PHONE-DESC, BM-PHONE-NO AND BM-PHONE-EXT TO BW
      * KEC 2016.0928 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         ADDED WARNING MESSAGE FOR CHANGE OF BW-BRANCH.
      * BAH 2016.1024 ADDED BW-BDATE-YYYYMMDD AND BW-SP-BDATE-YYYYMMDD,
      *               CHANGE FIELD 10 AND 37 TO NOW ENTER FULL DATE AND
      *               STORE IN THESE 2 NEW FIELDS, BUT STILL FILL IN THE
      *               OLD BW-BDATE AND BW-SP-BDATE WITH 6 DIGIT DATE
      * MJD 170301 ADDED BLANK ZERO TO DISPLAY BUF FOR F09 AND F10
      * BAH 170411 ACTIVATE 8 DIGIT DATES, PD0006
      * BAH 180207 DO NOT VALIDATE SPOUSE BDATE FOR MEXICO, THEY USE IT FOR
      *            LOAN APP DATE, WORLD PR#1304
      * BAH 180426 ADDED PASSWORD 15 FOR MEXICO TO NOT ALLOW CHANGES TO
      *            AN EXISTING CENSUS AND ORGSLSMN WHICH IS WHERE THEY STORE
      *            EMPLOYEE AND PROMOTER WHO GAVE THE LOAN #1316
      * BAH 180316 ADDED BI-LN-PLDATE TO BICREA CALL LIST, WORLD #1308
      * BAH 180925 CHANGED BW-BRANCH TO BW-ORGBR, L2G
      * BAH 190125 SPLIT CDFILE, G2G
      * BAH 190307 REMOVED BW-WORKHRS-NUM REDEFINES, WAS ONLY USED FOR MULLEN
      * BAH 190425 REMOVED MXCREA STUFF
      * BAH 20190709 REMOVED GP-LR-TRAILER-TRIAD, PT0151 
      * BAH 2020.0210 ONLY ALLOW VALID CODES FOR ORIGIN #1421
      * BAH 2021.0914 ASK PASSWORD TO CHANGE BIRTHDATE ONLY IF NOT 0, ALLOW
      *               THEM TO CHANGE IF 0 WITH NO PASSWORD. FORCE A VALID
      *               BIRTHDATE, #1513
      * BAH 2022.0314 REMOVED EXT-SPANISH-FORMS AND ALL MX FORMS
      * BAH 20230321 VERIFY-ZIPCODE #1596
      *****************************************************
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
           "@(#)UT/UTBORR.CBL S35 04/17/24-08:37:29 >".
      *    *      PROGRAM WORKERS
      *    **********************************
      *%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
       COPY "LIBUT/UT01UT.CPY".
       COPY "LIBUT/UT01UT_SQL.CPY".
      *%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
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


       01  ERRCD           PIC X      VALUE SPACES.

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

       01  LEGEND          PIC X(70).
       01  LEGEND-01.
           03  FILLER       PIC X(40)  VALUE
           " P#, F1-CANCEL, F2-MEMO, F3-LOANS, F7-UP".
           03  FILLER       PIC X(23)  VALUE
           "DATE, ENTER FIELD#:".

       01  LEGEND-NO-WALK       PIC X(35) VALUE
           "F1-CANCEL, ENTER FIELD INFORMATION:".
       01  LEGEND-NO-WALK-SCAN  PIC X(44) VALUE
           "F1-CANCEL, F6-SCAN, ENTER FIELD INFORMATION:".

       01  AGE-MESSAGE.
           03  FILLER                PIC X(43) VALUE
           "DATE OF BIRTH ENTERED MAKES BORROWER < 18. ".
           03  FILLER                PIC X(18) VALUE
           "PLEASE VERIFY DOB!".

       01  WORLD-OPT-LIST  PIC X(12) VALUE "P38:D.".
       01  WORLD-OPT-BUF   PIC X(13) VALUE SPACES.

       01  CHAN            PIC X(4).
       01  HOLD-SYS-DATE   PIC 9(8).
       01  BRANCH          PIC 9(4)   VALUE 0.
       01  ONE             PIC S9.
       01  ELE             PIC 99.

       01  ACTION.
           03  CHKEY       PIC X(4).
           03  CHNO        PIC 9(2).
       01  FORM-NUM1       REDEFINES ACTION.
           03  FORM-KEY1   PIC X(4).
           03  FORM-P1     PIC X.
           03  FORM-SEL1   PIC 9.

       01  ALLOW-CHANGES   PIC X      VALUE "Y".
       01  IO-TYPE         PIC X(3).
       01  NEW-FLAG        PIC 9.

      *=================================================================
      * ORG-BW-REC FROM 'REPLACING LEADING' ON BW-REC - ORIGINAL-BW-REC
      *=================================================================
       COPY "LIBLP/LP01BW.CPY" REPLACING LEADING "BW" BY "ORG-BW".
       01  CHANGED-BW-REC     PIC X(BW-SIZE).

       01  SPOUSE-EXISTS   PIC X VALUE "N".

       01  NAME            PIC X(16).
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

       01  ZIPCODE-FLAG    PIC X.
       01  TEST-ZIPCODE    PIC X(10).
       01  TEST-ZIP-GROUPS REDEFINES TEST-ZIPCODE.
           03  TEST-ZIP-5    PIC X(5).
           03  TEST-ZIP-DASH PIC X.
           03  TEST-ZIP-4    PIC X(4).

       01  ALLOW-CITY-SCAN-FG PIC X VALUE SPACES.
       01  ALLOW-COUNTY-SCAN-FG PIC X VALUE SPACES.
       01  DO-STATE-CT-EXIST-FG  PIC X VALUE SPACES.
       01  DO-STATE-CY-EXIST-FG  PIC X VALUE SPACES.

       01  ALLOW-BW-UPDATE-FG   PIC X  VALUE "N".
           88 ALLOW-BW-UPDATE-FG-OK VALUE "Y".
           88 ALLOW-BW-UPDATE-FG-BAD VALUE "N".

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
           03  F061        PIC X(10).
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
           03  F15         PIC XX.
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
           03  FILLER      PIC X(52) VALUE
           "090 100 110 120 130 140 150 160 161 170 180 190 200 ".
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
      * 7. HOME PHONE
           03  FILLER      PIC X(8) VALUE "E  A120 ".
      * 8. CELL PHONE
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
      *15. DWELLING CODE
           03  FILLER      PIC X(8) VALUE "E  A020 ".
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
      *37. SPOUSE'S BIRTH DATE ONLY
           03  FILLER      PIC X(8) VALUE "ERNM060 ".
      *38. PRIVACY OPT OUT (WORLD: TEXTING OPTION)
           03  FILLER      PIC X(8) VALUE "E  A010 ".
      *39. CALL ON SUNDAY
           03  FILLER      PIC X(8) VALUE "E  A010 ".
       01  SPEC-TAB        REDEFINES SPEC-TABLE.
           03  SPEC-REC    OCCURS 39 TIMES.
               05  SPEC    PIC X(7).
               05  CHFG    PIC X.


      ************(WINDOW INFORMATION):
       COPY "LIBCL/CLMENUW_EXT.CPY".
       COPY "LIBWI/PFSWINDOWSW.CPY".
       COPY "LIBWI/CHBORRW.CPY".
       COPY "LIBWI/MEMOSCW.CPY".
       COPY "LIBWI/BWLNS2W.CPY".
       COPY "LIBWI/CDSCANW.CPY".
       COPY "LIBWI/CTSCANW.CPY".
       COPY "LIBWI/CYSCANW.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBLP/LPWKPATH.CPY".
       COPY "LIBWI/CHBORR_CMD.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/GPENVW.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/PASSWDW.CPY".
       78  CRSCORE-SEQ    VALUE 0.
       78  REFDATE-SEQ    VALUE 1.
       78  CONFID-SEQ     VALUE 2.
      * THIS ONE PREVENTS REMOVING IT (OPTOUT)
       78  OPT-OUT-SEQ    VALUE 3.
       78  SMSA-SEQ       VALUE 4.
       78  LNAME-SEQ      VALUE 5.
       78  SOLICIT-SEQ    VALUE 6.
      * THIS ONE DOES NOT ALLOW ANY CHANGE
       78  OPT-OUT-SEQ2   VALUE 7.
       78  REFDATE-SEQ2   VALUE 8.
       78  NO-SPACES-SEQ  VALUE 9.
       78  MX-NAME-RFC    VALUE 10.
       78  REPCODE-SEQ    VALUE 11.
       78  FNAME-SEQ      VALUE 12.
      * ALL SPOUSE FIELDS(34 - 37) AND CITY CODE (21)
       78  CITY-SEQ       VALUE 13.
       78  BDATE-SEQ      VALUE 14.
       78  18-22-SEQ      VALUE 15.



       COPY "LIBGB/PHON400.CPY".
       COPY "LIBLP/LPBAGEW.CPY".
       01  CHBORR-WINDOW-HANDLE   PIC X(10).
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBWI/CHBORR_DEF.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBWI/CHBORR_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
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

       COPY "LIBGB/GETFMW.CPY".
       01  LINK-STATUS            PIC XX.
       01  LINK-BUF               PIC X(117).

       SCREEN SECTION.
       COPY "LIBWI/CHBORR_SCN.CPY".

       PROCEDURE DIVISION
             USING FORM-PATHNAME LINK-STATUS LINK-BUF EXIT-PATHNAME.

      ************************************
       MAIN-PROGRAM SECTION.
      ************************************

      *%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      *    MOVE SPACES TO UT-BWTABLEX.
           MOVE SPACES TO UT-MAKERCD(3).
           MOVE ZEROES TO UT-1STPYDA.
           IF ( UT-TEST3-GOOD )
              MOVE SPACES TO MESS.
           MOVE ZEROES TO UT-TEST-NUMERIC.
           MOVE "ABC"  TO UT-TEST-345A-ABCDEFGHIJK(1 1 1).
      *%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
           PERFORM SQL-CONNECT.

      D    STOP MESS.
           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "CHBORR" TO VDU-FORM-NAME.
           MOVE "WI/CHBORR" TO WINDOW-FORM-NAM.

       COPY "LIBGB/CHKSEC.CPY".

           MOVE LINK-BUF TO CHBORR-BUF.
           MOVE CHBORR-WORKER TO EXT-WI-CL-WORKER.

           MOVE " " TO VDU-WIMENU-SELECTION.

           MOVE EXT-WI-CL-SSNO(EXT-WI-CL-SSNO-NO) TO BW-SSNO.

           MOVE EXT-CONAME-DAILY-PASSWORD TO CHAN.

           MOVE EXT-CONAME-USER-LOC TO BRANCH.

           MOVE BRANCH TO BR-NO.
           PERFORM OPEN-BR-FILE.
           PERFORM READ-BR-FILE.
           PERFORM CLOSE-BR-FILE.


           DISPLAY FLOATING GRAPHICAL WINDOW
                   AT  SCREEN LINE CHBORR-BEGIN-X * VDU-WIN-L-OFFSET
                   AT  SCREEN COL  CHBORR-BEGIN-Y * VDU-WIN-C-OFFSET
                   CELL SIZE IS LABEL FONT
           COLOR IS VDU-WCOLOR-TCLP,CONTROL VALUE IS VDU-WCTRL-TCLP
                   SIZE    CHBORR-COLUMNS + VDU-WIN-W-MOD
                   LINES   CHBORR-LINES + VDU-WIN-H-MOD
                   BLANK SCREEN
                   HANDLE IS CHBORR-WINDOW-HANDLE.

           MOVE EXT-CONAME-SYSDATE TO ALP-DATE.
           PERFORM CONVERT-ALPDATE-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD TO HOLD-SYS-DATE.

           PERFORM GET-GPENV.

       MAIN-MODULE.

           PERFORM FORM-RESET.

           MOVE "38 TXT OPTIN:" TO WORLD-OPT-BUF.
           MOVE WORLD-OPT-BUF TO WR-BUF.
           MOVE WORLD-OPT-LIST TO WR-LIST..
           PERFORM CALL-WRFORM.

           PERFORM SEND-BW-MODULE.
           IF IO-TYPE = "END"
              GO TO END-ROUTINE.
           IF IO-TYPE = "CLR"
              GO TO MAIN-MODULE.

           PERFORM ENTRY-MODULE.
           IF IO-TYPE = "RWR"
              PERFORM WRITE-MODULE
              GO TO END-ROUTINE.
           IF IO-TYPE = "END"
              GO TO END-ROUTINE.

           GO TO MAIN-MODULE.

      **************************
       SEND-BW-MODULE SECTION.
      **************************
           MOVE SPACES TO IO-TYPE.
           MOVE 0 TO NEW-FLAG.

           MOVE "S  A000BWP." TO VDU.
           IF EXT-WI-CL-MAKERCD(EXT-WI-CL-SSNO-NO) = "M"
              MOVE "MAKER:" TO VDUBUF
           ELSE
              IF EXT-WI-CL-MAKERCD(EXT-WI-CL-SSNO-NO) = "J"
                 MOVE "JOINT:" TO VDUBUF
              ELSE
                 IF EXT-WI-CL-MAKERCD(EXT-WI-CL-SSNO-NO) = "E"
                    MOVE "ENDORSER:" TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "S  X110BWNO." TO VDU.
           MOVE BW-SSNO TO VDU-SSNO-EDIT.
           PERFORM SCREENIO.

       READ-BWFILE.
           MOVE BW-SSNO TO HOLD-KEY.
           PERFORM OPEN-BW1-FILE.
           PERFORM READ-BW1-FILE.
           PERFORM CLOSE-BW1-FILE.
           IF IO-FG = 0
              MOVE BW-REC TO ORG-BW-REC
              PERFORM DISPLAY-FIELDS
              MOVE 0 TO NEW-FLAG
           ELSE
              MOVE "BORROWER DOES NOT EXIST!" TO MESS
              PERFORM SEND-MESS
              MOVE "END" TO IO-TYPE.

       END-SEND-BW-MODULE.
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
           MOVE 0 TO ELE.
       CREATE-ROUTINE.

       MODIFY-ROUTINE.
           MOVE LEGEND-01 TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "R RA060SEL." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF VDUSTATUS = "16"
              MOVE "Y"        TO BWLNS2-SCAN-ONLY
              MOVE BW-SSNO    TO BWLNS2-KEY
              MOVE HOLD-SYS-DATE TO DATE-YYYYMMDD
              PERFORM CONVERT-YYYYMMDD-TO-ALPDATE
              MOVE ALP-DATE   TO BWLNS2-SYSDATE
              MOVE BWLNS2-BUF TO SCAN-BUF
              PERFORM CALL-WINDOW-PROGRAM
              GO TO MODIFY-ROUTINE.
           IF VDUSTATUS = "12"
              MOVE "B" TO MEMOSC-ONLY
              MOVE MEMOSC-BUF TO SCAN-BUF
              PERFORM CALL-WINDOW-PROGRAM
              GO TO MODIFY-ROUTINE.
           IF VDUSTATUS = "1>"
              PERFORM VALIDATE-REQUIRED-FIELD
              IF NOT (ALLOW-BW-UPDATE-FG-OK)
                 GO TO MODIFY-ROUTINE
              END-IF
              MOVE "RWR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
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
               GO TO MODIFY-ROUTINE.
           INSPECT ACTION REPLACING ALL SPACES BY ZEROS.
           IF CHNO NOT NUMERIC
              GO TO MODIFY-ROUTINE.
           IF CHNO = 0
              GO TO MODIFY-ROUTINE.
           IF CHNO > MAX
              GO TO MODIFY-ROUTINE.
           MOVE CHNO TO ELE.

      * CHANGE REFERENCE DATE ON AN EXISTING BORROWER
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

           IF ELE = 23
              IF NEW-FLAG = 0
                 MOVE REPCODE-SEQ TO PASSWDW-SEQ
                 MOVE "TO CHANGE REPCODE ..... ."
                                            TO PASSWDW-PROMPT2
                 PERFORM VERIFY-PASSWORD
                 IF PASSWDW-STAT NOT = "OK"
                    GO TO MODIFY-ROUTINE.

      * CITY CODE (21)
           IF ELE =  21 AND NEW-FLAG = 0
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
                 MOVE "TO OVERRIDE CONFIDENTIAL FLAG"
                                              TO PASSWDW-PROMPT2
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

      * WORLD MEXICO
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

      * CHANGE BIRTHDATE ON AN EXISTING BORROWER, IF NOT 0
           IF ELE = 10 AND NEW-FLAG = 0
              IF BW-BDATE NOT = 0
                 MOVE BDATE-SEQ TO PASSWDW-SEQ
                 MOVE "TO CHANGE BIRTHDATE...." TO PASSWDW-PROMPT2
                 PERFORM VERIFY-PASSWORD
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

       TEST-F6.
           IF NOT F6-KEY
              GO TO TEST-F5.

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
       TEST-F5.
           IF VDUSTATUS = "1D"
              MOVE 1 TO ONE
              GO TO GOTO-FIELD.
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
           GO TO CREATE-ROUTINE.
       F-02.
           MOVE VDUBUF TO BW-FNAME.
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
              IF BW-ADR1 = SPACES AND BW-ADR2 = SPACES
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
           MOVE VDUBUF TO UT-CODE.

           PERFORM READ-ST1-FILE.
           IF IO-FG = 9
             GO TO ENTER-ELE.

           PERFORM CLOSE-ST1-FILE.
           MOVE VDUBUF TO BW-STATE.
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
              PERFORM GET-CODE
              IF IO-FG = 9
                 MOVE "INVALID CODE; F6-SCAN CODES" TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-ELE.

           GO TO CREATE-ROUTINE.
       F-12.
           MOVE VDUNUM0 TO BW-DEPEND.
           GO TO CREATE-ROUTINE.
       F-13.
           MOVE VDUBUF TO CD-CODE BW-RENTCD.
           IF GP-BW-HOUSING-CODE NOT = "N"
              MOVE "BW" TO CD-TYPE
              PERFORM GET-CODE
              IF IO-FG = 9
                 MOVE "INVALID CODE; F6-SCAN CODES" TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-ELE.

           PERFORM TEST-RENTCD.
           IF IO-FG = 9
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.

      *F-13.
      *    MOVE VDUBUF TO BW-RENTCD.
      *    PERFORM TEST-RENTCD.
      *    IF IO-FG = 9
      *       GO TO ENTER-ELE.
      *    GO TO CREATE-ROUTINE.
       F-14.
           MOVE VDUBUF TO BW-TIMEADDR.
           GO TO CREATE-ROUTINE.
       F-15.
           MOVE VDUBUF TO BW-DWELLCD.
           GO TO CREATE-ROUTINE.
       F-16.
           MOVE VDUBUF TO BW-CREDITCD.
       F-161.
      *    -------------------------------------------------------------
      *    FORCE PASSWORD ON MODIFY OF EXISTING RECORD ONLY
      *    -------------------------------------------------------------
           IF NEW-FLAG = 0
              MOVE "TO CHANGE CREDIT SCORE" TO PASSWDW-PROMPT2
              MOVE CRSCORE-SEQ TO PASSWDW-SEQ
              PERFORM VERIFY-PASSWORD
              IF PASSWDW-STAT NOT = "OK"
                 GO TO CREATE-ROUTINE.


       F-161-BYPASS-PASSWORD.
           MOVE "ENNN040161." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO F-161-BYPASS-PASSWORD.
           MOVE VDUNUM0 TO BW-CREDIT-SCORE.
           IF GP-BW-CREDIT-SCORE = "Y"
              IF BW-CREDIT-SCORE NOT > 0
                 MOVE "MUST ENTER CREDIT SCORE" TO MESS
                 PERFORM SEND-MESS
                 GO TO F-161-BYPASS-PASSWORD.
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
           GO TO CREATE-ROUTINE.
       F-35.
           MOVE VDUBUF TO BW-SPFNAME.
           GO TO CREATE-ROUTINE.
       F-36.
           IF VDUNUM0 = HOLD-KEY
              MOVE "CAN NOT HAVE SAME SSNO AS CURRENT" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           MOVE "N" TO SPOUSE-EXISTS.
           PERFORM OPEN-BW1-FILE.
           MOVE BW-REC TO CHANGED-BW-REC.
           MOVE VDUNUM0 TO BW-SPSSNO NUM-BW.
           IF IO-FG NOT = 0
              GO TO ENTER-ELE.
           IF BW-SPSSNO = 0
              GO TO F-36-CONT.

           MOVE NUM-BW TO BW-SSNO.
           PERFORM READ-BW1-FILE.
           IF IO-FG = 0
              MOVE "Y" TO SPOUSE-EXISTS
              MOVE "SSNO ALREADY EXISTS UNDER THE NAME OF" TO MESS
              PERFORM SEND-MESS
              STRING BW-FNAME," ",BW-LNAME,
                                       DELIMITED BY "  " INTO MESS
              PERFORM SEND-MESS.
       F-36-CONT.
           PERFORM CLOSE-BW1-FILE.
           MOVE CHANGED-BW-REC TO BW-REC.

           MOVE NUM-BW TO BW-SPSSNO.
           MOVE "S  X110360." TO VDU.
           MOVE BW-SPSSNO TO VDU-SSNO-EDIT.
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
           MOVE VDUBUF TO BW-CALL-ON-SUNDAY.
           IF NOT (VDUBUF = "Y" OR "N")
              MOVE "Y OR N" TO MESS
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
                                ALLOW-CHANGES
                                CHAN.
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
                 MOVE "Y" TO BW-BAD-EMPPHONE-FG
              END-IF
              MOVE BW-MISC-PHONE(1) TO TEST-PHONE
              IF BAD-PHONE
                 MOVE "Y" TO BW-BAD-MISC-PHONE1-FG
              END-IF
              MOVE BW-MISC-PHONE(2) TO TEST-PHONE
              IF BAD-PHONE
                 MOVE "Y" TO BW-BAD-MISC-PHONE2-FG.

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
           GO TO END-WRITE-MODULE.

       END-WRITE-MODULE.
           PERFORM CLOSE-BW1-FILE.


      ************************************
       DISPLAY-FIELDS SECTION.
      ************************************
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
           MOVE BW-DWELLCD TO F15.
           MOVE BW-CREDITCD TO F16.
           MOVE BW-CREDIT-SCORE TO F161.
           MOVE BW-STRTGCD TO F17.
           MOVE BW-CENSUSCD TO F18.
           MOVE BW-ORGBR    TO F19.
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
           MOVE BW-CALL-ON-SUNDAY TO F39.

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


      ************************************
       CODE-SCAN SECTION.
      ************************************
           MOVE SPACES TO CDSCAN-CODE.

           MOVE CDSCAN-BUF TO SCAN-BUF.
           PERFORM CALL-WINDOW-PROGRAM.
           MOVE SCAN-BUF TO CDSCAN-BUF.

           IF (SCAN-STATUS NOT = "00") OR (CDSCAN-CODE  = SPACES)
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

      ************************************
       CYSCAN-SCAN SECTION.
      ************************************
           MOVE 40          TO CYSCAN-BEGIN-X.
           MOVE 4           TO CYSCAN-BEGIN-Y.

           MOVE SPACES      TO CYSCAN-CD1-KEY.
           MOVE "CY"        TO CYSCAN-KEY-TYPE.
           MOVE BW-STATE    TO CYSCAN-KEY-STATE.

           MOVE UT-DESC     TO CYSCAN-ST-DESC.
           MOVE CYSCAN-BUF  TO SCAN-BUF.
           PERFORM CALL-WINDOW-PROGRAM.
           MOVE SCAN-BUF    TO CYSCAN-BUF.

           IF ( SCAN-STATUS NOT = "00"   ) OR
              ( CYSCAN-KEY-CITY = SPACES )
              MOVE 9 TO IO-FG
              GO TO CYSCAN-SCAN-EXIT.

           MOVE "S  A030210."   TO VDU.
           MOVE CYSCAN-KEY-CITY TO VDUBUF.
           PERFORM SCREENIO.

           MOVE 0 TO IO-FG.
           MOVE CYSCAN-KEY-CITY TO VDUBUF.

       CYSCAN-SCAN-EXIT.
           EXIT.

      ************************************
       CTSCAN-SCAN SECTION.
      ************************************
           MOVE 10          TO CTSCAN-BEGIN-X.
           MOVE 4           TO CTSCAN-BEGIN-Y.
           MOVE SPACES      TO CTSCAN-CD1-KEY.
           MOVE "CT"        TO CTSCAN-KEY-TYPE.
           MOVE BW-STATE    TO CTSCAN-KEY-STATE.

           MOVE UT-DESC     TO CTSCAN-ST-DESC.
           MOVE CTSCAN-BUF  TO SCAN-BUF.
           PERFORM CALL-WINDOW-PROGRAM.
           MOVE SCAN-BUF    TO CTSCAN-BUF.

           IF ( SCAN-STATUS   NOT = "00"   ) OR
              ( CTSCAN-KEY-COUNTY = SPACES )
              MOVE 9 TO IO-FG
              GO TO CTSCAN-SCAN-EXIT.

           MOVE "S  A030200." TO VDU.
           MOVE CTSCAN-KEY-COUNTY TO VDUBUF.
           PERFORM SCREENIO.

           MOVE 0 TO IO-FG.
           MOVE CTSCAN-KEY-COUNTY TO VDUBUF.

       CTSCAN-SCAN-EXIT.
           EXIT.

      ************************************
       VERIFY-PASSWORD SECTION.
      ************************************
           MOVE " " TO PASSWDW-STAT.
           MOVE "CHBORR" TO PASSWDW-PGM.
           MOVE "PLEASE ENTER THE PASSWORD," TO PASSWDW-PROMPT1.
           MOVE "CL/PASSWD" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH PASSWDW-BUF.
           CANCEL FORM-PROGX.


      ************************************
       SET-CY-CT-SCAN SECTION.
      ************************************

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

      ************************************
       DO-STATE-CY-EXIST SECTION.
      ************************************

      * DETERMINE IF A VALID CD-CY-REC EXISTS FOR THE BORROWERS STATE

           MOVE " " TO DO-STATE-CY-EXIST-FG.

           PERFORM OPEN-CD1-FILE.

      * SETUP CD-CY-CITY BEFORE PERFORM DO-STATE-CY-EXIST
           MOVE "CY"       TO CD-TYPE.
           MOVE " "        TO CD-CY-FILLER.
           MOVE BW-STATE   TO CD-CY-STATE.

           PERFORM START-CD1-FILE.
           PERFORM READ-CD1-FILE-NEXT.
           IF IO-BAD
              GO TO DO-STATE-CY-EXIST-EXIT.

           IF CD-TYPE = "CY" AND CD-CY-STATE = BW-STATE
              MOVE "Y" TO DO-STATE-CY-EXIST-FG.

       DO-STATE-CY-EXIST-EXIT.
           PERFORM CLOSE-CD1-FILE.

      ************************************
       DO-STATE-CT-EXIST SECTION.
      ************************************

      * DETERMINE IF A VALID CD-CT-REC EXISTS FOR THE BORROWERS STATE

           MOVE " " TO DO-STATE-CT-EXIST-FG.

           PERFORM OPEN-CD1-FILE.

           MOVE "CT"       TO CD-TYPE.
      * SETUP CD-CT-COUNTY BEFORE PERFORM DO-STATE-CT-EXIST
           MOVE " "        TO CD-CT-FILLER.
           MOVE BW-STATE   TO CD-CT-STATE.

           PERFORM START-CD1-FILE.
           PERFORM READ-CD1-FILE-NEXT.
           IF IO-BAD
              GO TO DO-STATE-CT-EXIST-EXIT.

           IF CD-TYPE = "CT" AND CD-CT-STATE = BW-STATE
              MOVE "Y" TO DO-STATE-CT-EXIST-FG.

       DO-STATE-CT-EXIST-EXIT.
           PERFORM CLOSE-CD1-FILE.

      ********************************************************
       VALIDATE-REQUIRED-FIELD SECTION.
      ***********************************************************

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


       COPY "LIBGB/GPENV.CPY".
       COPY "LIBGB/DATER.CPY".
       COPY "LIBWI/PFSWINDOWS.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBLP/LPBAGE.CPY".

      ***********************************************
       FORM-RESET SECTION.
      ***********************************************
           DISPLAY SPACES UPON CHBORR-WINDOW-HANDLE
                          LINE 1 COL 1 ERASE SCREEN.
           DISPLAY CHBORR-SCREEN UPON CHBORR-WINDOW-HANDLE.
           MOVE CHBORR-FORM-FIELDS TO WR-BUF. 
           MOVE CHBORR-FORM-LIST TO WR-LIST. 
           PERFORM CALL-WRFORM.


       FORM-RESET-EXIT.
           EXIT.

      ***********************************************
       SETUP-LINK-INFO SECTION.
      ***********************************************
           MOVE CHBORR-HELP--FILE TO HELP-LINK-FILE.
           MOVE CHBORR-HELP--DIR TO HELP-LINK-DIR.

      ***********************************************
       VDU-CONVERT-FIELD SECTION.
      ***********************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBWI/CHBORR_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ***********************************************
       MISC-PARAGRAPHS SECTION.
      ***********************************************
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

           MOVE "CL/BICREA" TO FORM-NAM
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
       GET-CODE.
            PERFORM OPEN-CD1-FILE.
            PERFORM READ-CD1-FILE.
            PERFORM CLOSE-CD1-FILE.
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

      ***********************************************
       IO-ROUTINE SECTION.
      ***********************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-ST1-FILE.
           PERFORM CLOSE-CD1-FILE.
           PERFORM CLOSE-BX1-FILE.
           PERFORM CLOSE-BW1-FILE.
           PERFORM CLOSE-BR-FILE.
       COPY "LIBLP/LPSTGS_SQL.CPY".
       COPY "LIBLP/LPCDGS_SQL.CPY".
       COPY "LIBLP/LPBXGS_SQL.CPY".
       COPY "LIBLP/LPBWGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBLP/LPST1RN.CPY".
       COPY "LIBLP/LPBX1IN.CPY".
       COPY "LIBLP/LPBW1IN.CPY".
       COPY "LIBLP/LPCD1RN.CPY".
       COPY "LIBGB/GBBRRN.CPY".

       COPY "LIBGB/FERRORS.CPY".
       END-ROUTINE.
           PERFORM CLOSE-FILES.
       EXIT-PROG.
       STOP-RUN.
           MOVE CHBORR-BUF TO LINK-BUF.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY CHBORR-SCREEN.
           DESTROY CHBORR-WINDOW-HANDLE.
           EXIT PROGRAM.
