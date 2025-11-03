       IDENTIFICATION DIVISION.
       PROGRAM-ID.      BWMAN1.
      ********************************************************
      *   BORROWER MASTER FILE MAINTENANCE
      *              PAGE 1
      * REV:
      *  111990 BAH ADDED BW-RE-INTYTD BW-RE-INTLYR AND
      *             MADE 2 PAGES
      *  061391 BAH ADDED BW-EMPLADDR1 BW-EMPLADDR2 FOR MULLEN
      *  093096 JFF ADDED CODE SCAN FOR NEW CODE TYPE,
      *             CDTYPE-BW - BORROWER HOUSING CODE #13
      *  022497 BLV ADD BW-CREDIT-SCORE, FIELD 16-1
      *  031397 BAH ADDED GP-BW-HOUSING-CODE, MUST SET TO "N" TO
      *             IGNORE USING THE HOUSING CODES, #E141
      * KEC 041697 CHANGED ACCESS TO BW-REPCODE FROM NUMERIC TO ALPHA.
      * MJD 050997 ADDED GP-BW-CREDIT-SCORE. REQ#E155
      * JTG 082197 ADDED BW-OTHPHONE LOGIC WORLD A90
      * GLF 090997 REPLACED 'IO' ON GP-FILE WITH GET 'GPENV'
      * GLF 102397 REMOVED RESET OF 30 - 33
      * MJD 112497 EXPANDED BW-REC TO 480 BYTES
      * MJD 021998 ADDED BW-SIZE (78 LEVEL FOR REC SIZE)
      * GLF 030298 REMOVED BW-OTHPHONE
      * CS  102198 ADDED BW-CREDIT-LIMIT
      * GLF 062299 ADD PASSWORD TO CHANGE REFERENCE DATE ( WORLD PR#207 )
      * GLF 990909 CHANGE SSNO ENTRY TO USE NEW VDUCLASS "X" EDIT
      * MJD 000501 ADDED LOGIC FOR PAGE 3 (TFC PR#37)
      * BAH 001004 FORCE BW-REPCODE ENTRY TO "YES" OR "NO" FOR TAX PRE
      *            REGISTRATION, WORLD PR# 240
      * BAH 010627 ADDED BW-OPT-OUT, 37. OPT OUT :, TFC PR# 90
      *             ADDED OTHER MISSING PASSWORDS FROM MODBOR
      *               (CHANGING REFDATE, CONFIDENTIAL, CRSCORE)
      * BAH 010711 PASSWORD SMSA (BW-STRTGCD) FOR WORLD, PR# 269
      * CS  010801 DONT ALLOW ENTRY OF BW-PRIVACY-OPT-OUT FIELD IF
      *            GP-BW-PRIVACY-OPT-OUT NOT = "Y" PR#1571 ST LOUIS
      * CS  020215 IF AN EXISTING BORROWER WAS ACCESSED, REGARDLESS OF FORCE
      *            VALID-CITY-COUNTY THE COUNTY SCAN WAS  ALWAYS ALLOWED &
      *            CITY SCAN WAS NEVER ALLOWED ADDED SET-CY-CT-SCAN TO SET THES       *            FLAGS [WORLD #288]
      * CS  020218 ADDED VALIDATE OF CITY & COUNTY IF BR-FORCE-CITY-COUNTY = "Y"      *            BEFORE ALLOWING THE UPDATE [WORLD #288]
      * CS  020305 CORRECTED SETUP OF CD-BW-KEY
      * CS  020312 REMOVED EXTRA PERIOD IN SET-CY-CT-SCAN
      * BAH 020808 ADDED PASSWORD ON BW-LNAME, COLLECTORS ARE CHANGING
      *            LAST NAME TO GET THEM OUT OF THEIR QUEUE !! TFC PR# 94
      * BAH 021024 USERID-LIST WAS MISSING THE PERIOD "USERID.", CAUSING
      *            P2, THEN P1 TO GIVE A FORM ERROR 2P...JIMR
      *  CS 021203 IF BR-FORCE-VALID-CITY-COUNTY = "Y", GIVE A WARNING MESSAGE
      *            IF BW-CITY-TAX-CD = SPACES [WORLD PR#288]
      * BAH 030127 ADDED CHANGES LOG FOR ANY CHANGES....
      * BAH 030205 ADDED BW-CALL-ON-SUNDAY FIELD 39.  REGACC PR# 15
      * BAH 030410 ADDED BW-BAD-ADR-FG AND BW-BAD-PHONE-FG TRIAD REGACC PR# 26
      * BAH 030430 ADDED SET OF BW-ADR-PHONE-CHG-DATE FOR TRIAD REGACC PR# 26
      * BAH 030505 ADDED DISPLAY OF BW-ADR-PHONE-CHG-DATE FOR TRIAD PR# 26
      * BAH 030519 ADDED DISPLAY OF "ALT: Y" IF BXFILE HAS AN ALTERNATE
      *            ADDRESS IN IT, REGACC PR# 27
      * BAH 030728 ADDED BW-BAD-EMPPHONE-FG TRIAD REGACC PR# 36
      * BAH 031114 ADDED GP-USE-BW-BAD-FLAGS FOR THE ENTRY OF THE ABOVE REGACC
      *             FIELDS...SUPPORT PR# 2155
      * BAH 040218 AUTOMATICALLY SET BW-BAD-PHONE-FG FOR SPECIFIC
      *            PHONE NUMBERS, REGACC PR# 48
      * BAH 040409 CHANGE PROMPT 24 SOLICIT TO 24 CONTACT AND PROMPT 39 FROM
      *            CALL SUNDAY TO 39 OK SOLICIT FOR LENDMARK PR# 2271
      *            ALSO ADDED PASSWORDS FOR FIELD 24 AND 38, EVEN THOUGH
      *            CHANGES ARENT ALLOWED IN HERE...TO STAY CONSISTENT
      * MJD 070130 ADDED PW PROTECTION FOR REFDATE (NEW RECORD MODIFY)
      *            AND SKIPPED FIELD ON NEW RECORD CREATE IF REGENCY.
      *                                                    REGENCY PR#2856
      *  AMM 070323 CREATED A LOG FIELD CHANGE ROUTINE
      * BLF 080111 ADD CHECKS ON BORROWER NAME FIELDS, IF
      *            GP-EDIT-BORROWER-NAME-FG = Y, ONLY ALLOW A-Z, 0-9,
      *            SPACE, DASH OR APERSAND, WORLD PR # 564
      * BLM 080321 ALLOW APOSTROPHE IN NAME FIELDS IF GP-EDIT-BORROWER-NAME-FG
      *             = Y, REVISION TO WORLD PR# 564
      * BAH 080701 MOVE ZIP AND ADR BAD FLAG TO 6. STATE LINE
      *            MOVE HOME PHONE AND PH BAD FLAG FROM LINE 8 TO LINE 7
      *            ADD 8. CELL PHONE BW-CELL-PHONE        UNITED PR# 3205
      * BAH 090112 DONT ALLOW SAME SSNO FOR BORROWER AND SPOUSE
      *            DONT ALLOW SPOUSE SSNO IF NO SPOUSE NAME, WORLD (EAGLE PASS)
      * BAH 090520 CELL PHONE CHANGE WAS NOT GETTING LOGGED, SUPP PR# 3368
      * BAH 091005 PASSWORD CLEARING ADR,CITY,STATE,ZIP FOR REGACC PR# 256
      * BAH 100216 WAS NOT GOING TO ZIP IF YOU SPACES OUT THE STATE, WORLD
      * BAH 100608 ADDED BW-BAD-CELL-FG FIELD 081, REGACC PR# 266
      *  CS 110510 ADD TEST IF GP-I-AM-REGENCY & BR-STATE = "KY" FORCE KY TO
      *            CD-CY-STATE & CD-CT-STATE. REGENCY WANTS ALL CUSTOMERS IN
      *            KY BRANCH TO BE CHARGED INSURANCE TAX REGARDLESS OF
      *            BW-STATE   REGENCY PR#3784
      * BAH 120828 PASSWORD CHANGING FIRST NAME FNAME, REGMGM PR# 4100
      * BAH 120830 ADDED SET OF BW-DEALTRACK-CHG-DATE FOR REGACC PR# 319
      * BAH 120927 ADDED BW-WORKHRS-NUM FOR GP-I-AM-MULL, MULLEN PR# 4092
      * BAH 130819 CHANGE 38 OPT OUT FOR WORLD TO TXT OPTION, WORLD PR# 919
      * BAH 140225 CHANGE VALUES ALLOWED FOR WORLD TXT OPTION, WORLD PR# 970
      * BAH 140728 CHANGED SSNO ENTRY FROM 090 TO 110, "ERNX110BWNO."
      * BLM 141001 ALLOW VALUE OF "D" (DECLINED) FOR WORLD TXT OPTION 
      *            (BW-PRIVACY-OPT-OUT), WORLD PR# 1023
      * BAH 150903 FORCE BW-PRIVACY-OPT-OUT TO "N" IF CELL PHONE CHANGES
      *            AND WRITE A MEMO, WORLD PR# 1101
      * BAH 151216 ALLOW "C" IN BAD PHONE FLAGS, SOAUTO PR#4912
      * KEC 2016.0325 CHANGED CDSCAN-WINDOW-BUF TO HAVE MATCHING CD1-KEY
      *               LAYOUTS (FOR EACH CODE TYPE; CD-TYPE).
      *               CDSCAN-CD1-KEY IS THE NEW (PRIMARY) WITH REDEFINES OF
      *               CDSCAN-??-KEY (NEW).
      * KEC 2016.0328 CHANGED CYSCAN WORKERS TO MATCH CD1-KEY FOR CY LAYOUT.
      *               CHANGED CTSCAN WORKERS TO MATCH CD1-KEY FOR CT LAYOUT.
      * KEC 2016.0928 ADDED WARNING MESSAGE FOR CHANGE OF BW-ORGBR.
      * BAH 2016.1020 ADDED BW-BDATE-YYYYMMDD AND BW-SP-BDATE-YYYYMMDD,
      *               CHANGE FIELD 10 AND 37 TO NOW ENTER FULL DATE AND
      *               STORE IN THESE 2 NEW FIELDS, BUT STILL FILL IN THE
      *               OLD BW-BDATE AND BW-SP-BDATE WITH 6 DIGIT DATE
      * MJD 170301 ADDED BLANK ZERO TO DISPLAY BUF FOR F09 AND F10
      * BAH 170412 ACTIVATE 8 DIGIT DATES, PD0006
      *  CS 170413  MOVE VDU-DATE-YYYYMMDD TO BW-REFDATE.
      * BAH 180207 DO NOT VALIDATE SPOUSE BDATE FOR MEXICO, THEY USE IT FOR
      *            LOAN APP DATE, WORLD PR#1304
      * BAH 180426 ADDED PASSWORD 15 FOR MEXICO TO NOT ALLOW CHANGES TO
      *            AN EXISTING CENSUS AND ORGSLSMN WHICH IS WHERE THEY STORE
      *            EMPLOYEE AND PROMOTER WHO GAVE THE LOAN #1316
      * BAH 181127 GLOBAL LHFILE
      * BAH 181211 GLOBAL LNFILE
      * BAH 190128 SPLIT CDFILE
      * BAH 190307 REMOVED BW-WORKHRS-NUM REDEFINES, WAS ONLY USED FOR MULLEN
      * BAH 20190709 REMOVED GP-LR-TRAILER-TRIAD, PT0151 
      * BAH 20200210 ONLY ALLOW VALID CODES FOR ORIGIN #1421
      * BLM 20210924 FIX CAST ERROR IF ALPHA ENTERED IN SSNO SCAN
      * BLM 20210929 FIX ENTRY OF SPOUSE SSNO
      * BAH 20220803 HARD CODED START
      * BAH 20230321 VERIFY-ZIPCODE #1596
      * BAH 20230714 COMMENT OUT 3RD AND 4TH BORROWER TESTS, LN6 AND LN7
      * BAH 2024.1204 REPLACED CALL ON SUNDAY WITH BW-CEASE-DESIST-TYPE AND
      *               DONT ALLOW CHANGES TO FIELDS THAT ARE TIED TO IF THEY
      *               IN CEASE AND DESIST STATUS WITHOUT A PASSWORD #1686
      *                                                         S35Q-145
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       DATA DIVISION.
       FILE SECTION.

       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)MN/BWMAN1.CBL S35 02/21/24-12:34:32 >".

       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBLP/LP01BW.CPY".
       COPY "LIBLP/LP01BW_SQL.CPY".
       COPY "LIBLP/LPWSBW.CPY".
       COPY "LIBLP/LP01BX.CPY".
       COPY "LIBLP/LP01BX_SQL.CPY".
       COPY "LIBLP/LPWSBX.CPY".
       COPY "LIBLP/LP01ST.CPY".
       COPY "LIBLP/LP01ST_SQL.CPY".
       COPY "LIBLP/LPWSST.CPY".
       COPY "LIBLP/LP01BM.CPY".
       COPY "LIBLP/LP01BM_SQL.CPY".
       COPY "LIBLP/LPWSBM.CPY".
       COPY "LIBLP/LP01LN.CPY".
       COPY "LIBLP/LP01LN_SQL.CPY".
       COPY "LIBLP/LPWSLN.CPY".
       COPY "LIBLP/LP01LH.CPY".
       COPY "LIBLP/LP01LH_SQL.CPY".
       COPY "LIBLP/LPWSLH.CPY".
       COPY "LIBLP/LP01CD.CPY".
       COPY "LIBLP/LP01CD_SQL.CPY".
       COPY "LIBLP/LPWSCD.CPY".

      *=================================================================
      *   BWCH-REC FROM 'REPLACING LEADING' ON BW-REC. [CHANGED-BW-REC]
      *   BWOR-REC FROM 'REPLACING LEADING' ON BW-REC. [ORIGINAL-BW-REC]
      *=================================================================
       COPY "LIBLP/LP01BW.CPY" REPLACING LEADING "BW" BY "BWOR".
       COPY "LIBLP/LP01BW.CPY" REPLACING LEADING "BW" BY "BWCH".

      ******************************************************************
      *
      *    L M C - W O R K E R S
      *
      *=================================================================
      *    DESC: LOG-MAINTENANCE-CHGS WORKERS
      ******************************************************************
       01  LMC-DETAIL                  PIC X(55)      VALUE SPACES.
       01  FILLER                      REDEFINES LMC-DETAIL.
           03  LMC-BRNO                PIC 9(04).
           03  FILLER                  PIC X(01).
           03  LMC-FIELD               PIC X(02).
           03  FILLER                  PIC X(01).
           03  LMC-DESC                PIC X(12).
           03  FILLER                  PIC X(01).

           03  LMC-ORG-TEXT            PIC X(15).
           03  LMC-ORG-AMT             REDEFINES LMC-ORG-TEXT
                                       PIC ZZZ,ZZZ,ZZ9.99-.
           03  LMC-ORG-DATE            REDEFINES LMC-ORG-TEXT
                                       PIC 99/99/99.
           03  LMC-ORG-DLNO            REDEFINES LMC-ORG-TEXT
                                       PIC ZZZ9.99.
           03  LMC-ORG-NUM             REDEFINES LMC-ORG-TEXT
                                       PIC ZZZZZZZZZ9-.
           03  LMC-ORG-PHONE           REDEFINES LMC-ORG-TEXT
                                       PIC 999/999/9999.
           03  LMC-ORG-SSNO            REDEFINES LMC-ORG-TEXT
                                       PIC 999/99/9999.

           03  LMC-TO-X                PIC X(04).

           03  LMC-CHG-TEXT            PIC X(15).
           03  LMC-CHG-AMT             REDEFINES LMC-CHG-TEXT
                                       PIC ZZZ,ZZZ,ZZ9.99-.
           03  LMC-CHG-DATE            REDEFINES LMC-CHG-TEXT
                                       PIC 99/99/99.
           03  LMC-CHG-DLNO            REDEFINES LMC-CHG-TEXT
                                       PIC ZZZ9.99.
           03  LMC-CHG-NUM             REDEFINES LMC-CHG-TEXT
                                       PIC ZZZZZZZZZ9-.
           03  LMC-CHG-PHONE           REDEFINES LMC-CHG-TEXT
                                       PIC 999/999/9999.
           03  LMC-CHG-SSNO            REDEFINES LMC-CHG-TEXT
                                       PIC 999/99/9999.

      ******************************************************************

       01  SUB             PIC 999     VALUE 0.
       01  SUB2            PIC 999     VALUE 0.

       01  HOLD-SYS-DATE   PIC 9(8)    COMP-3.
       01  IX1             PIC 99      COMP-3.
       01  IX2             PIC 99      COMP-3.
       01  TYP             PIC 9       VALUE 0.
       01  ONE             PIC S9.
       01  ELE             PIC 9(2).
       01  ACTION.
           03  CHKEY       PIC X(4).
           03  CHNO        PIC 9(2).
           03  CHNO-X      REDEFINES CHNO
                           PIC X(2).
       01  FORM-NUM1       REDEFINES ACTION.
           03  FORM-KEY1   PIC X(4).
           03  FORM-P1     PIC X.
           03  FORM-SEL1   PIC 9.

       01  IO-TYPE         PIC X(3).
       01  HOLD-IO-FG      PIC 99.
       01  NEW-FLAG        PIC 9.
       01  FIRST-SCAN      PIC 9       VALUE 0.
       01  NAME            PIC X(16).
       01  HOLD-KEY        PIC 9(9).
       01  SOCSEC-KEY      PIC 9(9).

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

       01  SAVE-BM1-KEY.
           03  SAVE-BM-BRNO    PIC 9(4).
           03  SAVE-BM-SSNO    PIC 9(9).
           03  SAVE-BM-DATE    PIC 9(8).
           03  SAVE-BM-SEQ     PIC 9(3).

       01  MEMO-AREA.
           03  FILLER          PIC X(5)  VALUE "OLD: ".
           03  MEMO-DESCRIPT   PIC X(20).
           03  FILLER          PIC X     VALUE " ".
           03  MEMO-ORG-CHAR   PIC X(24).
           03  FILLER          PIC X     VALUE " ".
           03  MEMO-USERID     PIC X(10).

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

       01  WORK-NAME           PIC X(16).
       01  FILLER              REDEFINES WORK-NAME.
           03  NAME-CHAR       PIC X OCCURS 16 TIMES.
       01  TEST-CHAR           PIC X.
           88 VALID-CHAR       VALUE " " "-" "&" "'".
       01  BAD-NAME-FG         PIC X    VALUE "N".

       01  ALLOW-BW-UPDATE-FG   PIC X  VALUE "N".
           88 ALLOW-BW-UPDATE-FG-OK VALUE "Y".
           88 ALLOW-BW-UPDATE-FG-BAD VALUE "N".

       01  BW-ALP-TABLE.
           03  BW-ALP-TBL  OCCURS 70 TIMES
                           PIC X.
      *******************************************


       01  BWMAIN-LINK       VALUE SPACES.
      * LEFT SAVEKEY JUST SSNO, SAVEKEY IS MOVED TO SOCSEC-KEY 
           03  SAVEKEY       PIC X(9).
           03  NEW-REC       PIC X.
           03  ERRCD         PIC X.
           03  FORMSEL       PIC 99.
           03  BWMAIN-WINDOW-HANDLE PIC X(10).

       01  WORLD-OPT-LIST  PIC X(12) VALUE "P38:D.".
       01  WORLD-OPT-BUF   PIC X(13) VALUE SPACES.

      *    **********************************
      *    *      SCREEN DISPLAY BUFFER
      *    **********************************
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
      * 7. HOME PHONE
           03  F07         PIC X(12).
      * BAD PHONE FLAG
           03  F071        PIC X.
      * BW-ADR-PHONE-CHG-DATE
           03  F072        PIC 99/99/99 BLANK ZERO.
      * 8. CELL PHONE
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
      * CEASE & DESIST TYPE
           03  F39         PIC X.

       01  DISPLAY-LIST.
           03  FILLER      PIC X(56) VALUE
           "010 020 030 040 050 051 052 060 061 062 070 071 072 080 ".
           03  FILLER      PIC X(56) VALUE
           "081 090 100 110 120 130 140 141 150 160 161 170 180 190 ".
           03  FILLER      PIC X(56) VALUE
           "200 210 220 230 240 250 260 270 280 290 300 301 302 310 ".
           03  FILLER      PIC X(40) VALUE
           "311 320 330 331 340 350 360 370 380 390.".

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
      * 6. STATE    6.1 ZIP   6.2 ADR BAD
           03  FILLER      PIC X(8) VALUE "E  A020 ".
      * 7. PHONE
           03  FILLER      PIC X(8) VALUE "E  A120 ".
      * 8. CELL PHONE
           03  FILLER      PIC X(8) VALUE "E  A120 ".
      * 9. REFERENCE DATE
           03  FILLER      PIC X(8) VALUE "ERNM060 ".
      *10. AGE OR BIRTHDATE BDATE
           03  FILLER      PIC X(8) VALUE "ERND080 ".
      *11. ORIGIN
           03  FILLER      PIC X(8) VALUE "E  A040 ".
      *12. DEPENDENTS
           03  FILLER      PIC X(8) VALUE "EN N020 ".
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
           03  FILLER      PIC X(8) VALUE "ENNN040 ".
      *20. COUNTY CODE
           03  FILLER      PIC X(8) VALUE "E  A030 ".
      *21. CITY CODE
           03  FILLER      PIC X(8) VALUE "E  A030 ".
      *22. ORIGINAL SALESMAN
           03  FILLER      PIC X(8) VALUE "ENNN040 ".
      *23. REP CODE
      *--->03  FILLER      PIC X(8) VALUE "EN N040 ".
           03  FILLER      PIC X(8) VALUE "E  A040 ".
      *24. SOLICIT CODE
           03  FILLER      PIC X(8) VALUE "E  A010 ".
      *25. CONFIDENTIAL
           03  FILLER      PIC X(8) VALUE "E  A010 ".
      *26. EMPLOYER'S NAME
           03  FILLER      PIC X(8) VALUE "E  A240 ".
      *27. EMPLOYER'S ADDR1
           03  FILLER      PIC X(8) VALUE "E  A240 ".
      *28. EMPLOYER'S ADDR2
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
      *37. SPOUSE'S AGE/BIRTH BDATE
           03  FILLER      PIC X(8) VALUE "ERND080 ".
      *38. PRIVACY OPT OUT (WORLD: TEXTING OPTION)
           03  FILLER      PIC X(8) VALUE "E  A010 ".
      *39. CEASE AND DESIST TYPE
           03  FILLER      PIC X(8) VALUE "E  A010 ".

       01  SPEC-TAB       REDEFINES SPEC-TABLE.
           03  SPEC-REC   OCCURS 39 TIMES.
               05  SPEC    PIC X(7).
               05  CHFG    PIC X.

       COPY "LIBWI/CDSCANW.CPY".
       COPY "LIBGB/GPENVW.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/PASSWDW.CPY".
       78  CRSCORE-SEQ           VALUE 0.
       78  REFDATE-SEQ           VALUE 1.
       78  CONFID-SEQ            VALUE 2.
      * THIS ONE PROVENTS REMOVING IT (OPTOUT)
       78  OPT-OUT-SEQ           VALUE 3.
       78  SMSA-SEQ              VALUE 4.
       78  LNAME-SEQ             VALUE 5.
       78  SOLICIT-SEQ           VALUE 6.
      * THIS ONE DOES NOT ALLOW ANY CHANGE
       78  OPT-OUT-SEQ2          VALUE 7.
       78  REFDATE-SEQ2          VALUE 8.
       78  NO-SPACES-SEQ         VALUE 9.
       78  FNAME-SEQ             VALUE 12.
       78  18-22-SEQ             VALUE 15.
      * USED IN BWCRE2
       78  PHONE-16              VALUE 16.
       78  CEASE-DESIST-SEQ      VALUE 17.

       COPY "LIBWI/CYSCANW.CPY".
       COPY "LIBWI/CTSCANW.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/PHON400.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBMN/BWMAN1_DEF.CPY".
       COPY "LIBMN/BWMAN1_WKS.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".

       COPY "LIBGB/GETFMW.CPY".
       01  FORM-LINK-AREA      PIC X(2000).

       SCREEN SECTION.
       COPY "LIBMN/BWMAN1_SCN.CPY".

       PROCEDURE DIVISION
           USING FORM-PATHNAME FORM-LINK-AREA EXIT-PATHNAME.

      ********************************************
       MAIN-PROGRAM SECTION.
      ********************************************

           PERFORM SQL-CONNECT.

      D    STOP MESS.

           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "BWMAN1" TO VDU-FORM-NAME.

           MOVE FORM-LINK-AREA TO BWMAIN-LINK.

           MOVE EXT-CONAME-SYSDATE TO ALP-DATE.
           PERFORM CONVERT-ALPDATE-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD TO HOLD-SYS-DATE.

           MOVE " " TO VDU-WIMENU-SELECTION
                       VDU-PGMENU-SELECTION.

           PERFORM GET-GPENV.

           MOVE EXT-CONAME-USER-LOC TO BR-NO.
           PERFORM OPEN-BR-FILE.
           PERFORM READ-BR-FILE.
           PERFORM CLOSE-BR-FILE.

       MAIN-MODULE.
           PERFORM FORM-RESET.

           PERFORM REC-BW-MODULE.
           IF IO-TYPE = "END"
              MOVE "X" TO ERRCD
              GO TO END-ROUTINE.
           IF IO-TYPE = "CLR"
              GO TO MAIN-MODULE.

           PERFORM ENTRY-MODULE.
           IF IO-TYPE = "RWR"
              PERFORM WRITE-MODULE.
           IF IO-TYPE = "DEL"
              PERFORM DELETE-MODULE.

      *    IF NEW-REC = "Y"
      *       MOVE "P" TO FORM-P1
      *       MOVE 2 TO FORM-SEL1.
      * DO NOT GO TO PAGE AUTOMATICALLY ON NEW RECORD
      * THEY ARE PROTECTED FIELDS
           IF FORM-SEL1 NOT = 0
              IF FORM-P1 = "P"
                 MOVE BW-SSNO TO SAVEKEY
                 MOVE FORM-SEL1 TO FORMSEL
                 GO TO END-ROUTINE.

           MOVE "N" TO NEW-REC.
           GO TO MAIN-MODULE.

      ***********************************
       REC-BW-MODULE SECTION.
      ***********************************
       REC-BW-MODULE-NO.
           MOVE SPACES TO IO-TYPE.
           IF SAVEKEY NOT = " "
              MOVE SAVEKEY TO BW-SSNO SOCSEC-KEY HOLD-KEY
              MOVE SPACES TO SAVEKEY
              MOVE "S  X110BWNO." TO VDU
              MOVE BW-SSNO TO VDU-SSNO-EDIT
              PERFORM SCREENIO
              GO TO READ-BWFILE.
           MOVE 0 TO NEW-FLAG.
       REC-BW-SSNO.
           MOVE "ERNX110BWNO." TO VDU.
      *    MOVE "ERNX090BWNO." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>"
              MOVE "END" TO IO-TYPE
              GO TO END-REC-BW-MODULE.
           IF VDUSTATUS = "1<"
              PERFORM ENTER-TYPEKEY
              GO TO END-REC-BW-MODULE.
           IF VDUSTATUS NOT = "00"
              GO TO REC-BW-SSNO.
           MOVE VDUNUM0 TO BW-SSNO HOLD-KEY.

           IF BW-SSNO = ZERO
              GO TO REC-BW-MODULE-NO.

       READ-BWFILE.
           PERFORM OPEN-BW1-FILE.
           PERFORM READ-BW1-FILE.
           PERFORM CLOSE-BW1-FILE.
           IF IO-FG = 0
              MOVE BW-REC TO BWOR-REC
              PERFORM DISPLAY-FIELDS
              MOVE 0 TO NEW-FLAG
              GO TO END-REC-BW-MODULE.

           PERFORM NEW-RECORD-SETUP.
       END-REC-BW-MODULE.
           EXIT.

      *    ************************************************
      *    *      ENTER SORTKEY ROUTINES
      *    *      TYPE = 1 - BORROWER SOCIAL SEC. #
      *    *             2 - BORROWER NAME
      *    *             3 - SPOUSE SOCIAL SEC. #
      *    *             4 - SPOUSE NAME
      *    ************************************************
       ENTER-TYPEKEY SECTION.
       ENTER-TYPE.
      D    STOP MESS.
           PERFORM OPEN-BW1-FILE.

           MOVE BW-PATH-OWNBR TO BW-OWNBR.

           MOVE " 1=SS#, 2=NAME, 3=SPOUSE SS#, 4=SPOUSE NAME, F6=SCAN"
              TO MESS.
           PERFORM CALL-TMMSG.
           MOVE "EN N010TYPE." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10"
             MOVE "CLR" TO IO-TYPE
             GO TO CLOSE-SORTKEY.
           IF VDUSTATUS = "1<"
              MOVE "SN N010TYPE." TO VDU
              MOVE 1 TO TYP VDUNUM0
              PERFORM SCREENIO
              PERFORM DISPLAY-SCAN-TYPE
              GO TO READ-FILES-NEXT.
           IF VDUSTATUS NOT = "00"
             GO TO ENTER-TYPE.
           MOVE VDUNUM0 TO TYP.
           IF TYP < 1 OR TYP > 4
             GO TO ENTER-TYPE.
       DISPLAY-SCAN-TYPE.
           IF TYP = 1
              MOVE 0 TO BW-SSNO
              PERFORM START-BW1-FILE
              MOVE " ENTER SS#, OR F6 TO SCAN" TO MESS
              PERFORM CALL-TMMSG.
           IF TYP = 2
              MOVE SPACES TO BW-NAME
              PERFORM START-BW2-FILE
              MOVE " ENTER NAME (LAST), OR F6 TO SCAN" TO MESS
              PERFORM CALL-TMMSG.
           IF TYP = 3
              MOVE 0 TO BW-SPSSNO
              PERFORM START-BW3-FILE
              MOVE " ENTER SPOUSE SS#, OR F6 TO SCAN" TO MESS
              PERFORM CALL-TMMSG.
           IF TYP = 4
              MOVE SPACES TO BW-SPNAME
              PERFORM START-BW4-FILE
              MOVE " ENTER SPOUSE NAME (LAST), OR F6 TO SCAN" TO MESS
              PERFORM CALL-TMMSG.
       ENTER-KEY.
           IF TYP = 2 OR TYP = 4
              MOVE "E RA240SORTKEY." TO VDU
           ELSE
              MOVE "E RA110SORTKEY." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10"
             MOVE "CLR" TO IO-TYPE
             GO TO CLOSE-SORTKEY.
           IF VDUSTATUS = "1U"
             PERFORM CLOSE-SORTKEY
             GO TO ENTER-TYPE.
           IF VDUSTATUS = "1<"
             GO TO READ-FILES-NEXT.
           IF VDUSTATUS NOT = "00"
             GO TO ENTER-KEY.
           IF VDUBUF = SPACES
              IF FIRST-SCAN NOT = 0
                 MOVE BW-REC TO BWOR-REC
                 GO TO CLOSE-SORTKEY
              ELSE
                 GO TO ENTER-KEY.
           IF TYP = 1 OR TYP = 3
              INSPECT VDUBUF REPLACING ALL " " BY "0"
              MOVE VDUBUF TO SOCSEC-KEY
              IF SOCSEC-KEY NOT NUMERIC
                 GO TO ENTER-KEY.
           IF TYP = 1 OR TYP = 3
              MOVE VDUBUF TO BW-SSNO BW-SPSSNO.
           IF TYP = 2 OR TYP = 4
              PERFORM ARRANGE-BW-KEY
              MOVE BW-ALP-TABLE TO BW-NAME BW-SPNAME.
           IF TYP = 1 PERFORM START-BW1-FILE.
           IF TYP = 2 PERFORM START-BW2-FILE.
           IF TYP = 3 PERFORM START-BW3-FILE.
           IF TYP = 4 PERFORM START-BW4-FILE.
       READ-FILES-NEXT.
           IF TYP = 1 PERFORM READ-BW1-FILE-NEXT.
      *               UNLOCK BW-FILE.
           IF TYP = 2 PERFORM READ-BW2-FILE-NEXT.
           IF TYP = 3 PERFORM READ-BW3-FILE-NEXT.
           IF TYP = 4 PERFORM READ-BW4-FILE-NEXT.
           IF (IO-FG = 9) OR (BW-OWNBR NOT = BW-PATH-OWNBR)
             MOVE "CLR" TO IO-TYPE
             GO TO CLOSE-SORTKEY.

           MOVE "S  X110BWNO." TO VDU.
           MOVE BW-SSNO TO VDU-SSNO-EDIT HOLD-KEY.
           PERFORM SCREENIO.

           PERFORM DISPLAY-FIELDS.

           MOVE 1 TO FIRST-SCAN.
           GO TO ENTER-KEY.
       CLOSE-SORTKEY.
           IF TYP = 1 PERFORM CLOSE-BW1-FILE.
           IF TYP = 2 PERFORM CLOSE-BW2-FILE.
           IF TYP = 3 PERFORM CLOSE-BW3-FILE.
           IF TYP = 4 PERFORM CLOSE-BW4-FILE.
       END-SORTKEY.
           MOVE 0 TO FIRST-SCAN.
           MOVE SPACES TO MESS.
           PERFORM CALL-TMMSG.
           MOVE "S  A010TYPE." TO VDU.
           PERFORM SCREENIO.
           MOVE "S  A160SORTKEY." TO VDU.
           PERFORM SCREENIO.

      ***********************************
       ARRANGE-BW-KEY SECTION.
      ***********************************
           MOVE 1 TO IX1 IX2.
           MOVE SPACES TO BW-ALP-TABLE.
       ARR-BW-KEY-AGAIN.
           IF IX1 NOT < 35
              GO TO ARR-BW-KEY-EXIT.
           IF VDUTAB(IX1) NOT = ","
              MOVE VDUTAB(IX1) TO BW-ALP-TBL(IX2)
            ELSE
              MOVE 35 TO IX2.
           ADD 1 TO IX1.
           ADD 1 TO IX2.
           GO TO ARR-BW-KEY-AGAIN.
       ARR-BW-KEY-EXIT.
           EXIT.

      *    ************************************************
      *    *   ENTRY-MODULE
      *    *   NEW-FLAG = 0 - MODIFY EXISTING RECORD
      *    *              1 - INITIAL NEW RECORD ENTRY
      *    *              2 - MODIFY NEW RECORD
      *    ************************************************
       ENTRY-MODULE SECTION.
       BEGIN-ENTRY.
           MOVE SPACES TO IO-TYPE.
           MOVE 1 TO ONE.
           MOVE 0 TO ELE.
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
           MOVE "R RA060SEL." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF VDUSTATUS NOT = "00"
              GO TO MODIFY-ROUTINE.
           MOVE VDUBUF TO ACTION.
           IF ACTION = SPACES
              PERFORM VALIDATE-REQUIRED-FIELD
              IF NOT (ALLOW-BW-UPDATE-FG-OK)
                 GO TO MODIFY-ROUTINE
              END-IF
              MOVE "RWR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF ACTION = "DELETE"
              PERFORM VERIFY-DELETE
      * COULD NOT DELETE A SSNO OF ALL 0'S FROM A CONVERSION...
              IF IO-FG = 9 AND BW-SSNO NOT = 0
                 GO TO MODIFY-ROUTINE
              ELSE
                 MOVE "DEL" TO IO-TYPE
                 GO TO END-ENTRY-MODULE.
           IF FORM-KEY1 = " " AND FORM-P1 = "P" AND FORM-SEL1 NUMERIC
              IF FORM-SEL1 < 0 OR = 1
                 GO TO MODIFY-ROUTINE
              ELSE
                 IF NOT (FORM-SEL1 = 2 OR 3 OR 4)
                    GO TO MODIFY-ROUTINE
                 ELSE
                    MOVE "RWR" TO IO-TYPE
                    GO TO EXIT-ENTRY-MODULE.
           INSPECT ACTION REPLACING ALL SPACES
              BY ZEROS.
           IF CHNO-X NOT NUMERIC
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


      * TO REMOVE PRIVACY OPT OUT ON AN EXISTING BORROWER
      *    IF ELE = 38
      * WORLD USING OPT OUT FOR TEXT OPTION
      *       IF NOT GP-I-AM-WORLD
      *          IF GP-BW-PRIVACY-OPT-OUT = "Y"
      *             IF BW-PRIVACY-OPT-OUT = "Y"
      *                MOVE OPT-OUT-SEQ TO PASSWDW-SEQ
      *                MOVE "TO REMOVE OPT OUT.....:" TO PASSWDW-PROMPT2
      *                PERFORM VERIFY-PASSWORD
      *                IF PASSWDW-STAT NOT = "OK"
      *                   GO TO MODIFY-ROUTINE.

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
              IF CHKEY NOT = EXT-CONAME-DAILY-PASSWORD
                 GO TO MODIFY-ROUTINE.


      *    IF ELE = 38
      * WORLD USING OPT OUT FOR TEXT OPTION
      *       IF NOT GP-I-AM-WORLD
      *          IF GP-BW-PRIVACY-OPT-OUT NOT = "Y"
      *             IF NEW-FLAG = 0
      *                GO TO CREATE-ROUTINE
      *             ELSE
      *                GO TO MODIFY-ROUTINE.

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
           IF VDUSTATUS NOT = "1<"
              GO TO TEST-F1.

           IF ELE = 11
              MOVE "OR"   TO CDSCAN-TYPE
              PERFORM CODE-SCAN
              IF IO-FG = 0
                 MOVE 1 TO ONE
                 GO TO GOTO-FIELD
              ELSE
                 GO TO ENTER-ELE.
           IF ELE = 13
              MOVE "BW"   TO CDSCAN-TYPE
              PERFORM CODE-SCAN
              IF IO-FG = 0
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
           IF ELE = 20 AND ALLOW-COUNTY-SCAN-FG = "Y"
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
           MOVE VDUBUF TO ST-CODE BW-STATE.
           PERFORM READ-ST1-FILE.
           IF IO-FG = 9
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
           IF BWOR-PRIVACY-OPT-OUT = "M" OR "B" OR "A"
              IF BW-CELL-PHONE NOT = BWOR-CELL-PHONE
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

           IF (BW-BDATE NOT = 0)
              MOVE BW-BDATE TO NUM-DATE
              PERFORM TSTDAT
              IF NUM-DATE = 0
                 MOVE "INVALID DATE " TO MESS
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
           IF GP-BW-HOUSING-CODE = "N"
              GO TO CREATE-ROUTINE.

           MOVE "BW" TO CD-TYPE.
           PERFORM GET-CD.
           IF HOLD-IO-FG = 9
              MOVE "INVALID CODE; F6-SCAN CODES" TO MESS
              PERFORM SEND-MESS
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
       F-161.

      * FORCE PASSWORD ON MODIFY OF EXISTING RECORD ONLY
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
           IF VDUBUF NOT = "YES" AND VDUBUF NOT = "NO"
              MOVE "'YES' OR 'NO'" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       F-24.
           MOVE VDUBUF TO BW-SOLICIT.
           IF VDUBUF NOT = "Y" AND VDUBUF NOT = "N"
              MOVE "Y OR N" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       F-25.
           MOVE VDUBUF TO BW-CONFIDENTIAL.
           IF VDUBUF NOT = "Y" AND VDUBUF NOT = "N"
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
           IF TEL-BUFX IS NOT NUMERIC
               GO TO ENTER-ELE.
           MOVE TEL-BUF TO BW-EMPPHONE.
           PERFORM SEND-PHONE.
           MOVE "S  A120300." TO VDU.
           MOVE "-" TO TEL-H1 TEL-H2.
           MOVE TEL-EDIT TO VDUBUF.
           PERFORM SCREENIO.

       F-301.
           MOVE "EN N040301." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO F-301.
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
              GO TO F-301.
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
           IF VDUNUM0 NOT = 0
              IF BW-SPFNAME = SPACES OR BW-SPLNAME = SPACES
                 MOVE "MUST HAVE SPOUSE NAME TO ENTER A SSNO" TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-ELE.

           MOVE VDUNUM0 TO BW-SPSSNO.
           GO TO CREATE-ROUTINE.
       F-37.
           MOVE VDU-DATE-YYYYMMDD TO BW-SP-BDATE.

           IF (BW-SP-BDATE NOT = 0)
              MOVE BW-SP-BDATE TO NUM-DATE
              PERFORM TSTDAT
              IF NUM-DATE = 0
                 MOVE "INVALID DATE " TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-ELE.

           GO TO CREATE-ROUTINE.
       F-38.
           MOVE VDUBUF TO BW-PRIVACY-OPT-OUT.

           IF NOT (VDUBUF = "A" OR "M" OR "B" OR "N" OR "S" OR "D")
              MOVE "ENTER: 'A', 'M', 'B', 'N', 'S' OR 'D' ONLY"
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
       WRITE-MODULE SECTION.
      ***************************************
           PERFORM OPEN-BW1-FILE.
           IF NEW-FLAG NOT = 0
              GO TO WRITE-RECORD.

           MOVE BW-REC TO BWCH-REC.

           IF BWOR-REC = BWCH-REC
              GO TO END-WRITE-MODULE.

           PERFORM READ-BW1-FILE.
           IF IO-FG NOT = 0
              PERFORM UNDERGONE-CHANGE
              PERFORM RECORD-DELETED
              GO TO END-WRITE-MODULE.

           IF BW-REC NOT = BWOR-REC
              PERFORM UNDERGONE-CHANGE
              PERFORM CHANGE-AGAIN
              GO TO END-WRITE-MODULE.

           PERFORM LOG-MAINTENANCE-CHGS.

           MOVE BWCH-REC TO BW-REC.

      * REGACC PR# 319
           IF BWOR-FNAME NOT = BW-FNAME OR
               BWOR-LNAME NOT = BW-LNAME OR
                BWOR-ADR1 NOT = BW-ADR1 OR
                 BWOR-ADR2 NOT = BW-ADR2 OR
                  BWOR-CITY NOT = BW-CITY OR
                   BWOR-STATE NOT = BW-STATE OR
                    BWOR-ZIP NOT = BW-ZIP OR
                     BWOR-PHONE NOT = BW-PHONE OR
                      BWOR-EMPPHONE NOT = BW-EMPPHONE
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

           GO TO END-WRITE-MODULE.
       WRITE-RECORD.
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
              PERFORM ALREADY-CREATED
           ELSE
              MOVE "Y" TO NEW-REC.
       END-WRITE-MODULE.
           PERFORM CLOSE-BW1-FILE.

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

      ******************************************************************
       LOG-MAINTENANCE-CHGS SECTION.
      ******************************************************************

           MOVE SPACES                     TO LMC-DETAIL.
           MOVE BR-NO                      TO LMC-BRNO.
           MOVE " TO "                     TO LMC-TO-X.

           IF BWOR-LNAME NOT = BWCH-LNAME
             MOVE "01 "                   TO LMC-FIELD
             MOVE "BW LNAME"              TO LMC-DESC
             MOVE BWOR-LNAME              TO LMC-ORG-TEXT
             MOVE BWCH-LNAME              TO LMC-CHG-TEXT
             PERFORM LOG-FIELD-CHANGE.

           IF BWOR-FNAME NOT = BWCH-FNAME
             MOVE "02 "                   TO LMC-FIELD
             MOVE "BW FNAME"              TO LMC-DESC
             MOVE BWOR-FNAME              TO LMC-ORG-TEXT
             MOVE BWCH-FNAME              TO LMC-CHG-TEXT
             PERFORM LOG-FIELD-CHANGE.

           IF BWOR-ADR1 NOT = BWCH-ADR1
             MOVE "03 "                   TO LMC-FIELD
             MOVE "ADR1    "              TO LMC-DESC
             MOVE BWOR-ADR1               TO LMC-ORG-TEXT
             MOVE BWCH-ADR1               TO LMC-CHG-TEXT
             PERFORM LOG-FIELD-CHANGE.

           IF BWOR-ADR2 NOT = BWCH-ADR2
             MOVE "04 "                   TO LMC-FIELD
             MOVE "ADR2    "              TO LMC-DESC
             MOVE BWOR-ADR2               TO LMC-ORG-TEXT
             MOVE BWCH-ADR2               TO LMC-CHG-TEXT
             PERFORM LOG-FIELD-CHANGE.

           IF BWOR-CITY NOT = BWCH-CITY
             MOVE "05 "                   TO LMC-FIELD
             MOVE "BW CITY "              TO LMC-DESC
             MOVE BWOR-CITY               TO LMC-ORG-TEXT
             MOVE BWCH-CITY               TO LMC-CHG-TEXT
             PERFORM LOG-FIELD-CHANGE.

           IF BWOR-STATE NOT = BWCH-STATE
             MOVE "06 "                   TO LMC-FIELD
             MOVE "BW STATE"              TO LMC-DESC
             MOVE BWOR-STATE              TO LMC-ORG-TEXT
             MOVE BWCH-STATE              TO LMC-CHG-TEXT
             PERFORM LOG-FIELD-CHANGE.

           IF BWOR-ZIP NOT = BWCH-ZIP
             MOVE "061"                   TO LMC-FIELD
             MOVE "BW ZIP  "              TO LMC-DESC
             MOVE BWOR-ZIP                TO LMC-ORG-TEXT
             MOVE BWCH-ZIP                TO LMC-CHG-TEXT
             PERFORM LOG-FIELD-CHANGE.

           IF BWOR-BAD-ADR-FG NOT = BWCH-BAD-ADR-FG
             MOVE "062"                   TO LMC-FIELD
             MOVE "BADADRFG"              TO LMC-DESC
             MOVE BWOR-BAD-ADR-FG         TO LMC-ORG-TEXT
             MOVE BWCH-BAD-ADR-FG         TO LMC-CHG-TEXT
             PERFORM LOG-FIELD-CHANGE.

           IF BWOR-PHONE NOT = BWCH-PHONE
             MOVE "07 "                   TO LMC-FIELD
             MOVE "PHONE   "              TO LMC-DESC
             MOVE BWOR-PHONE              TO LMC-ORG-PHONE
             INSPECT LMC-ORG-PHONE REPLACING ALL "/" BY "-"
             MOVE BWCH-PHONE              TO LMC-CHG-PHONE
             INSPECT LMC-CHG-PHONE REPLACING ALL "/" BY "-"
             PERFORM LOG-FIELD-CHANGE.

           IF BWOR-BAD-PHONE-FG NOT = BWCH-BAD-PHONE-FG
             MOVE "071"                   TO LMC-FIELD
             MOVE "BADPHONE"              TO LMC-DESC
             MOVE BWOR-BAD-PHONE-FG       TO LMC-ORG-TEXT
             MOVE BWCH-BAD-PHONE-FG       TO LMC-CHG-TEXT
             PERFORM LOG-FIELD-CHANGE.

           IF BWOR-ADR-PHONE-CHG-DATE NOT = BWCH-ADR-PHONE-CHG-DATE
             MOVE "072"                   TO LMC-FIELD
             MOVE "PHONE CH"              TO LMC-DESC
             MOVE BWOR-ADR-PHONE-CHG-DATE TO DATE-YYYYMMDD
             PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
             MOVE DATE-MMDDYY             TO LMC-ORG-DATE
             MOVE BWCH-ADR-PHONE-CHG-DATE TO DATE-YYYYMMDD
             PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
             MOVE DATE-MMDDYY             TO LMC-CHG-DATE
             PERFORM LOG-FIELD-CHANGE.

           IF BWOR-CELL-PHONE NOT = BWCH-CELL-PHONE
             MOVE "08 "                   TO LMC-FIELD
             MOVE "CELPHONE"              TO LMC-DESC
             MOVE BWOR-CELL-PHONE         TO LMC-ORG-PHONE
             INSPECT LMC-ORG-PHONE REPLACING ALL "/" BY "-"
             MOVE BWCH-CELL-PHONE         TO LMC-CHG-PHONE
             INSPECT LMC-CHG-PHONE REPLACING ALL "/" BY "-"
             PERFORM LOG-FIELD-CHANGE.

           IF BWOR-BAD-CELL-FG NOT = BWCH-BAD-CELL-FG
             MOVE "081"                   TO LMC-FIELD
             MOVE "BAD CELL"              TO LMC-DESC
             MOVE BWOR-BAD-CELL-FG       TO LMC-ORG-TEXT
             MOVE BWCH-BAD-CELL-FG       TO LMC-CHG-TEXT
             PERFORM LOG-FIELD-CHANGE.

           IF BWOR-REFDATE NOT = BWCH-REFDATE
             MOVE "09 "                   TO LMC-FIELD
             MOVE "REFDATE "              TO LMC-DESC
             MOVE BWOR-REFDATE            TO DATE-YYYYMMDD
             PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
             MOVE DATE-MMDDYY             TO LMC-ORG-DATE
             MOVE BWCH-REFDATE            TO DATE-YYYYMMDD
             PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
             MOVE DATE-MMDDYY             TO LMC-CHG-DATE
             PERFORM LOG-FIELD-CHANGE.

           IF BWOR-BDATE NOT = BWCH-BDATE
             MOVE "10 "                   TO LMC-FIELD
             MOVE "BDATE   "              TO LMC-DESC
             MOVE BWOR-BDATE              TO DATE-YYYYMMDD
             PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
             MOVE DATE-MMDDYY             TO LMC-ORG-DATE
             MOVE BWCH-BDATE              TO DATE-YYYYMMDD
             PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
             MOVE DATE-MMDDYY             TO LMC-CHG-DATE
             PERFORM LOG-FIELD-CHANGE.

           IF BWOR-ORIGIN NOT = BWCH-ORIGIN
             MOVE "11 "                   TO LMC-FIELD
             MOVE "ORIGIN  "              TO LMC-DESC
             MOVE BWOR-ORIGIN             TO LMC-ORG-TEXT
             MOVE BWCH-ORIGIN             TO LMC-CHG-TEXT
             PERFORM LOG-FIELD-CHANGE.

           IF BWOR-DEPEND NOT = BWCH-DEPEND
             MOVE "12 "                   TO LMC-FIELD
             MOVE "DEPEND  "              TO LMC-DESC
             MOVE BWOR-DEPEND             TO LMC-ORG-NUM
             MOVE BWCH-DEPEND             TO LMC-CHG-NUM
             PERFORM LOG-FIELD-CHANGE.

           IF BWOR-RENTCD NOT = BWCH-RENTCD
             MOVE "13 "                   TO LMC-FIELD
             MOVE "RENTCD  "              TO LMC-DESC
             MOVE BWOR-RENTCD             TO LMC-ORG-TEXT
             MOVE BWCH-RENTCD             TO LMC-CHG-TEXT
             PERFORM LOG-FIELD-CHANGE.

           IF BWOR-TIMEADDR NOT = BWCH-TIMEADDR
             MOVE "14 "                   TO LMC-FIELD
             MOVE "TIMEADDR"              TO LMC-DESC
             MOVE BWOR-TIMEADDR           TO LMC-ORG-TEXT
             MOVE BWCH-TIMEADDR           TO LMC-CHG-TEXT
             PERFORM LOG-FIELD-CHANGE.

           IF BWOR-DWELLCD NOT = BWCH-DWELLCD
             MOVE "141"                   TO LMC-FIELD
             MOVE "DWELLCD "              TO LMC-DESC
             MOVE BWOR-DWELLCD            TO LMC-ORG-TEXT
             MOVE BWCH-DWELLCD            TO LMC-CHG-TEXT
             PERFORM LOG-FIELD-CHANGE.

           IF BWOR-CREDIT-LIMIT NOT = BWCH-CREDIT-LIMIT
             MOVE "15 "                   TO LMC-FIELD
             MOVE "CRED LIM"              TO LMC-DESC
             MOVE BWOR-CREDIT-LIMIT       TO LMC-ORG-AMT
             MOVE BWCH-CREDIT-LIMIT       TO LMC-CHG-AMT
             PERFORM LOG-FIELD-CHANGE.

           IF BWOR-CREDITCD NOT = BWCH-CREDITCD
             MOVE "16 "                   TO LMC-FIELD
             MOVE "CREDITCD"              TO LMC-DESC
             MOVE BWOR-CREDITCD           TO LMC-ORG-TEXT
             MOVE BWCH-CREDITCD           TO LMC-CHG-TEXT
             PERFORM LOG-FIELD-CHANGE.

           IF BWOR-CREDIT-SCORE NOT = BWCH-CREDIT-SCORE
             MOVE "161"                   TO LMC-FIELD
             MOVE "CRED SCR"              TO LMC-DESC
             MOVE BWOR-CREDIT-SCORE       TO LMC-ORG-NUM
             MOVE BWCH-CREDIT-SCORE       TO LMC-CHG-NUM
             PERFORM LOG-FIELD-CHANGE.

           IF BWOR-STRTGCD NOT = BWCH-STRTGCD
             MOVE "17 "                   TO LMC-FIELD
             MOVE "STRTGCD "              TO LMC-DESC
             MOVE BWOR-STRTGCD            TO LMC-ORG-TEXT
             MOVE BWCH-STRTGCD            TO LMC-CHG-TEXT
             PERFORM LOG-FIELD-CHANGE.

           IF BWOR-CENSUSCD NOT = BWCH-CENSUSCD
             MOVE "18 "                   TO LMC-FIELD
             MOVE "CENSUSCD"              TO LMC-DESC
             MOVE BWOR-CENSUSCD           TO LMC-ORG-TEXT
             MOVE BWCH-CENSUSCD           TO LMC-CHG-TEXT
             PERFORM LOG-FIELD-CHANGE.

           IF BWOR-ORGBR NOT = BWCH-ORGBR
             MOVE "19 "                   TO LMC-FIELD
             MOVE "BRANCH  "              TO LMC-DESC
             MOVE BWOR-ORGBR             TO LMC-ORG-NUM
             MOVE BWCH-ORGBR             TO LMC-CHG-NUM
             PERFORM LOG-FIELD-CHANGE.

           IF BWOR-COUNTY NOT = BWCH-COUNTY
             MOVE "20 "                   TO LMC-FIELD
             MOVE "COUNTY  "              TO LMC-DESC
             MOVE BWOR-COUNTY             TO LMC-ORG-TEXT
             MOVE BWCH-COUNTY             TO LMC-CHG-TEXT
             PERFORM LOG-FIELD-CHANGE.

           IF BWOR-CITY-TAX-CD NOT = BWCH-CITY-TAX-CD
             MOVE "21 "                   TO LMC-FIELD
             MOVE "CITYTAXC"              TO LMC-DESC
             MOVE BWOR-CITY-TAX-CD        TO LMC-ORG-TEXT
             MOVE BWCH-CITY-TAX-CD        TO LMC-CHG-TEXT
             PERFORM LOG-FIELD-CHANGE.

           IF BWOR-ORGSLSMN NOT = BWCH-ORGSLSMN
             MOVE "22 "                   TO LMC-FIELD
             MOVE "ORGSLSMN"              TO LMC-DESC
             MOVE BWOR-ORGSLSMN           TO LMC-ORG-NUM
             MOVE BWCH-ORGSLSMN           TO LMC-CHG-NUM
             PERFORM LOG-FIELD-CHANGE.

           IF BWOR-REPCODE NOT = BWCH-REPCODE
             MOVE "23 "                   TO LMC-FIELD
             MOVE "REPCODE "              TO LMC-DESC
             MOVE BWOR-REPCODE            TO LMC-ORG-TEXT
             MOVE BWCH-REPCODE            TO LMC-CHG-TEXT
             PERFORM LOG-FIELD-CHANGE.

           IF BWOR-SOLICIT NOT = BWCH-SOLICIT
             MOVE "24 "                   TO LMC-FIELD
             MOVE "SOLICIT "              TO LMC-DESC
             MOVE BWOR-SOLICIT            TO LMC-ORG-TEXT
             MOVE BWCH-SOLICIT            TO LMC-CHG-TEXT
             PERFORM LOG-FIELD-CHANGE.

           IF BWOR-CONFIDENTIAL NOT = BWCH-CONFIDENTIAL
             MOVE "25 "                   TO LMC-FIELD
             MOVE "CONFIDEN"              TO LMC-DESC
             MOVE BWOR-CONFIDENTIAL       TO LMC-ORG-TEXT
             MOVE BWCH-CONFIDENTIAL       TO LMC-CHG-TEXT
             PERFORM LOG-FIELD-CHANGE.

           IF BWOR-EMPLOYER NOT = BWCH-EMPLOYER
             MOVE "26 "                   TO LMC-FIELD
             MOVE "EMPLOYER"              TO LMC-DESC
             MOVE BWOR-EMPLOYER           TO LMC-ORG-TEXT
             MOVE BWCH-EMPLOYER           TO LMC-CHG-TEXT
             PERFORM LOG-FIELD-CHANGE.

           IF BWOR-EMPADDR1 NOT = BWCH-EMPADDR1
             MOVE "27 "                   TO LMC-FIELD
             MOVE "EMPADDR1"              TO LMC-DESC
             MOVE BWOR-EMPADDR1           TO LMC-ORG-TEXT
             MOVE BWCH-EMPADDR1           TO LMC-CHG-TEXT
             PERFORM LOG-FIELD-CHANGE.

           IF BWOR-EMPADDR2 NOT = BWCH-EMPADDR2
             MOVE "28 "                   TO LMC-FIELD
             MOVE "EMPADDR2"              TO LMC-DESC
             MOVE BWOR-EMPADDR2           TO LMC-ORG-TEXT
             MOVE BWCH-EMPADDR2           TO LMC-CHG-TEXT
             PERFORM LOG-FIELD-CHANGE.

           IF BWOR-EMPLPOS NOT = BWCH-EMPLPOS
             MOVE "29 "                   TO LMC-FIELD
             MOVE "EMPLPOS "              TO LMC-DESC
             MOVE BWOR-EMPLPOS            TO LMC-ORG-TEXT
             MOVE BWCH-EMPLPOS            TO LMC-CHG-TEXT
             PERFORM LOG-FIELD-CHANGE.

           IF BWOR-EMPPHONE NOT = BWCH-EMPPHONE
             MOVE "30 "                   TO LMC-FIELD
             MOVE "EMPPHONE"              TO LMC-DESC
             MOVE BWOR-EMPPHONE           TO LMC-ORG-PHONE
             INSPECT LMC-ORG-PHONE REPLACING ALL "/" BY "-"
             MOVE BWCH-EMPPHONE           TO LMC-CHG-PHONE
             INSPECT LMC-CHG-PHONE REPLACING ALL "/" BY "-"
             PERFORM LOG-FIELD-CHANGE.

           IF BWOR-EMPEXT NOT  = BWCH-EMPEXT
             MOVE "301"                   TO LMC-FIELD
             MOVE "EMPEXT  "              TO LMC-DESC
             MOVE BWOR-EMPEXT             TO LMC-ORG-NUM
             MOVE BWCH-EMPEXT             TO LMC-CHG-NUM
             PERFORM LOG-FIELD-CHANGE.

           IF BWOR-BAD-EMPPHONE-FG NOT = BWCH-BAD-EMPPHONE-FG
             MOVE "302"                   TO LMC-FIELD
             MOVE "BADEMPPH"              TO LMC-DESC
             MOVE BWOR-BAD-EMPPHONE-FG    TO LMC-ORG-TEXT
             MOVE BWCH-BAD-EMPPHONE-FG    TO LMC-CHG-TEXT
             PERFORM LOG-FIELD-CHANGE.

           IF BWOR-EMPYEARS NOT = BWCH-EMPYEARS
             MOVE "31 "                   TO LMC-FIELD
             MOVE "EMPYEARS"              TO LMC-DESC
             MOVE BWOR-EMPYEARS           TO LMC-ORG-NUM
             MOVE BWCH-EMPYEARS           TO LMC-CHG-NUM
             PERFORM LOG-FIELD-CHANGE.

           IF BWOR-WORKHRS NOT = BWCH-WORKHRS
             MOVE "311"                   TO LMC-FIELD
             MOVE "WORKHRS "              TO LMC-DESC
             MOVE BWOR-WORKHRS            TO LMC-ORG-TEXT
             MOVE BWCH-WORKHRS            TO LMC-CHG-TEXT
             PERFORM LOG-FIELD-CHANGE.

           IF BWOR-EMPPAY NOT = BWCH-EMPPAY
             MOVE "32 "                   TO LMC-FIELD
             MOVE "EMPPAY  "              TO LMC-DESC
             MOVE BWOR-EMPPAY             TO LMC-ORG-AMT
             MOVE BWCH-EMPPAY             TO LMC-CHG-AMT
             PERFORM LOG-FIELD-CHANGE.

           IF BWOR-EMPPAYOT NOT = BWCH-EMPPAYOT
             MOVE "33 "                   TO LMC-FIELD
             MOVE "EMPPAYOT"              TO LMC-DESC
             MOVE BWOR-EMPPAYOT           TO LMC-ORG-AMT
             MOVE BWCH-EMPPAYOT           TO LMC-CHG-AMT
             PERFORM LOG-FIELD-CHANGE.

           IF BWOR-EMPPAYSR NOT = BWCH-EMPPAYSR
             MOVE "331"                   TO LMC-FIELD
             MOVE "EMPPAYSR"              TO LMC-DESC
             MOVE BWOR-EMPPAYSR           TO LMC-ORG-AMT
             MOVE BWCH-EMPPAYSR           TO LMC-CHG-AMT
             PERFORM LOG-FIELD-CHANGE.

           IF BWOR-SPLNAME NOT = BWCH-SPLNAME
             MOVE "34 "                   TO LMC-FIELD
             MOVE "SPLNAME "              TO LMC-DESC
             MOVE BWOR-SPLNAME            TO LMC-ORG-TEXT
             MOVE BWCH-SPLNAME            TO LMC-CHG-TEXT
             PERFORM LOG-FIELD-CHANGE.

           IF BWOR-SPFNAME NOT = BWCH-SPFNAME
             MOVE "35 "                   TO LMC-FIELD
             MOVE "SPFNAME "              TO LMC-DESC
             MOVE BWOR-SPFNAME            TO LMC-ORG-TEXT
             MOVE BWCH-SPFNAME            TO LMC-CHG-TEXT
             PERFORM LOG-FIELD-CHANGE.

           IF BWOR-SPSSNO NOT = BWCH-SPSSNO
             MOVE "36 "                   TO LMC-FIELD
             MOVE "SPSSNO  "              TO LMC-DESC
             MOVE BWOR-SPSSNO             TO LMC-ORG-SSNO
             INSPECT LMC-ORG-SSNO REPLACING ALL "/" BY "-"
             MOVE BWCH-SPSSNO             TO LMC-CHG-SSNO
             INSPECT LMC-CHG-SSNO REPLACING ALL "/" BY "-"
             PERFORM LOG-FIELD-CHANGE.

           IF BWOR-SP-BDATE NOT = BWCH-SP-BDATE
             MOVE "37 "                   TO LMC-FIELD
             MOVE "SP-BIRTH-DATE   "      TO LMC-DESC
             MOVE BWOR-SP-BDATE           TO DATE-YYYYMMDD
             PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
             MOVE DATE-MMDDYY             TO LMC-ORG-DATE
             MOVE BWCH-SP-BDATE           TO DATE-YYYYMMDD
             PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
             MOVE DATE-MMDDYY             TO LMC-CHG-DATE
             PERFORM LOG-FIELD-CHANGE.

           IF BWOR-PRIVACY-OPT-OUT NOT = BWCH-PRIVACY-OPT-OUT
             MOVE "38 "                   TO LMC-FIELD
             MOVE "PRIV OPT"              TO LMC-DESC
             MOVE BWOR-PRIVACY-OPT-OUT    TO LMC-ORG-TEXT
             MOVE BWCH-PRIVACY-OPT-OUT    TO LMC-CHG-TEXT
             PERFORM LOG-FIELD-CHANGE
             PERFORM CREATE-MEMO.

           IF BWOR-CEASE-DESIST-TYPE NOT = BWCH-CEASE-DESIST-TYPE
             MOVE "39 "                      TO LMC-FIELD
             MOVE "CEASE   "                 TO LMC-DESC
             MOVE BWOR-CEASE-DESIST-TYPE     TO LMC-ORG-TEXT
             MOVE BWCH-CEASE-DESIST-TYPE     TO LMC-CHG-TEXT
             PERFORM LOG-FIELD-CHANGE.

       LOG-MAINTENANCE-CHGS-EXIT.
           EXIT.

      ******************************************************************
       LOG-FIELD-CHANGE SECTION.
      ******************************************************************

           MOVE "F"           TO ERRLOGW-ACTION.
           MOVE LMC-DETAIL    TO ERRLOGW-DETAIL.
           MOVE FORM-PATHNAME TO ERRLOGW-PROGX.

           MOVE "CL/ERRLOG" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH ERRLOGW-AREA.
           CANCEL FORM-PROGX.

       LOG-FIELD-CHANGE-EXIT.
           EXIT.

      ******************************************************************
       CREATE-MEMO SECTION.
      ******************************************************************

           PERFORM OPEN-BM1-FILE.

           MOVE BM-PATH-OWNBR TO SAVE-BM-BRNO.
           MOVE BW-SSNO       TO SAVE-BM-SSNO.
           COMPUTE SAVE-BM-DATE = 99999999 - HOLD-SYS-DATE.
           MOVE 1             TO SAVE-BM-SEQ.
       CREATE-MEMO-AGAIN.
           MOVE SPACES TO BM-REC.

           MOVE SAVE-BM1-KEY         TO BM1-KEY.
           MOVE EXT-CONAME-USER-ID   TO MEMO-USERID.
           MOVE "TEXT OPTION"        TO MEMO-DESCRIPT.
           MOVE BWOR-PRIVACY-OPT-OUT TO MEMO-ORG-CHAR.
           MOVE MEMO-AREA TO BM-MEMO.

           MOVE "9" TO BM-MEMO-TYPE.
           PERFORM WRITE-BM1-FILE.
           IF IO-BAD
              IF SAVE-BM-SEQ < 999
                 ADD 1 TO SAVE-BM-SEQ
                 GO TO CREATE-MEMO-AGAIN.

           PERFORM CLOSE-BM1-FILE.

      *********************************************
       DELETE-MODULE SECTION.
      *********************************************
           IF NEW-FLAG NOT = 0
              GO TO END-DELETE-MODULE.

           PERFORM OPEN-BW1-FILE.
           PERFORM READ-BW1-FILE.
           IF IO-FG NOT = 0
              PERFORM UNDERGONE-CHANGE
              PERFORM RECORD-DELETED
              GO TO END-DELETE-MODULE.
           IF BW-REC NOT = BWOR-REC
              PERFORM UNDERGONE-CHANGE
              PERFORM RECORD-CHANGED
              GO TO END-DELETE-MODULE.

           PERFORM DELETE-BW1-FILE.

      *WRITE CHANGES FILE
           MOVE "C" TO ERRLOGW-ACTION.
           MOVE " " TO ERRLOGW-KEY
           STRING BW-SSNO, " ", "DELETED" DELIMITED BY SIZE
                INTO ERRLOGW-KEY.

           MOVE FORM-PATHNAME TO ERRLOGW-PROGX.

           MOVE "CL/ERRLOG" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH ERRLOGW-AREA.
           CANCEL FORM-PROGX.

           PERFORM OPEN-BM1-FILE.

           MOVE BM-PATH-OWNBR TO BM-BRNO
                                 QBM1-WBEG-BRNO
                                 QBM1-WEND-BRNO.
           MOVE BW-SSNO       TO BM-SSNO
                                 QBM1-WBEG-SSNO
                                 QBM1-WEND-SSNO.
           MOVE 0             TO BM-DATE
                                 QBM1-WBEG-DATE-REV
                                 BM-SEQ 
                                 QBM1-WBEG-SEQ.
           MOVE ALL "9"       TO QBM1-WEND-DATE-REV
                                 QBM1-WEND-SEQ.
           PERFORM START-BM1-FILE.

       DELETE-BW-MEMOS.
           PERFORM READ-BM1-FILE-NEXT.
           IF (IO-FG NOT = 0) OR (BM-BRNO NOT = BM-PATH-OWNBR)
              GO TO END-DELETE-MODULE.

           IF BW-SSNO NOT = BM-SSNO
              GO TO END-DELETE-MODULE.

           PERFORM DELETE-BM1-FILE.

           GO TO DELETE-BW-MEMOS.
       END-DELETE-MODULE.
           PERFORM CLOSE-BM1-FILE.
           PERFORM CLOSE-BW1-FILE.

      *******************************************
       NEW-RECORD-SETUP SECTION.
      *******************************************

           MOVE 1 TO NEW-FLAG.
           MOVE "S  A000NEW." TO VDU.
           MOVE "*** NEW RECORD ***" TO VDUBUF.
           PERFORM SCREENIO.
       CLEAR-NEW-RECORD.
           PERFORM CLEAR-BW-FILE.
           MOVE BW-PATH-OWNBR TO BW-OWNBR.
           MOVE HOLD-KEY      TO BW-SSNO.
           MOVE 0 TO ELE.
           MOVE "S  A040230." TO VDU.
           MOVE "NO" TO VDUBUF BW-REPCODE.
           PERFORM SCREENIO.

      **********************************************
       DISPLAY-FIELDS SECTION.
      **********************************************

           PERFORM SET-CY-CT-SCAN.

           MOVE SPACES TO DISPLAY-BUF.

           MOVE BW-LNAME        TO F01.
           MOVE BW-FNAME        TO F02.
           MOVE BW-ADR1         TO F03.
           MOVE BW-ADR2         TO F04.
           MOVE BW-CITY         TO F05.
           MOVE BW-STATE        TO F06.
           MOVE BW-ZIP          TO F061.
           MOVE BW-BAD-ADR-FG   TO F062.
           IF BW-PHONE NOT = 0
              MOVE BW-PHONE TO TEL-BUF
              PERFORM SEND-PHONE
              MOVE TEL-EDIT     TO F07.
           MOVE BW-BAD-PHONE-FG TO F071.
           MOVE BW-ADR-PHONE-CHG-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY     TO F072.
           IF BW-CELL-PHONE NOT = 0
              MOVE BW-CELL-PHONE TO TEL-BUF
              PERFORM SEND-PHONE
              MOVE TEL-EDIT     TO F08.
           MOVE BW-BAD-CELL-FG  TO F081.
           MOVE BW-REFDATE      TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY     TO F09.
           MOVE BW-BDATE        TO DATE-YYYYMMDD
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYYYY
           MOVE DATE-MMDDYYYY   TO F10.
           MOVE BW-ORIGIN       TO F11.
           MOVE BW-DEPEND       TO F12.
           MOVE BW-RENTCD       TO F13.
           MOVE BW-TIMEADDR     TO F14.
           MOVE BW-DWELLCD      TO F141.
           MOVE BW-CREDIT-LIMIT TO F15.
           MOVE BW-CREDITCD     TO F16.
           MOVE BW-CREDIT-SCORE TO F161.
           MOVE BW-STRTGCD      TO F17.
           MOVE BW-CENSUSCD     TO F18.
           MOVE BW-ORGBR        TO F19.
           MOVE BW-COUNTY       TO F20.
           MOVE BW-CITY-TAX-CD  TO F21.
           MOVE BW-ORGSLSMN     TO F22.
           MOVE BW-REPCODE      TO F23.
           MOVE BW-SOLICIT      TO F24.
           MOVE BW-CONFIDENTIAL TO F25.
           MOVE BW-EMPLOYER     TO F26.
           MOVE BW-EMPADDR1     TO F27.
           MOVE BW-EMPADDR2     TO F28.
           MOVE BW-EMPLPOS      TO F29.
           IF BW-EMPPHONE NOT = 0
              MOVE BW-EMPPHONE TO TEL-BUF
              PERFORM SEND-PHONE
              MOVE TEL-EDIT     TO F30.
           MOVE BW-EMPEXT       TO F301.
           MOVE BW-BAD-EMPPHONE-FG TO F302.
           MOVE BW-EMPYEARS     TO F31.
           MOVE BW-WORKHRS      TO F311.
           MOVE BW-EMPPAY       TO F32.
           MOVE BW-EMPPAYOT     TO F33.
           MOVE BW-EMPPAYSR     TO F331.
           MOVE BW-SPLNAME      TO F34.
           MOVE BW-SPFNAME      TO F35.
           IF BW-SPSSNO > ZERO
              MOVE BW-SPSSNO    TO F36
              INSPECT F36 REPLACING ALL "/" BY "-".
           MOVE BW-SP-BDATE        TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYYYY.
           MOVE DATE-MMDDYYYY      TO F37.
           MOVE BW-PRIVACY-OPT-OUT TO F38.
           MOVE BW-CEASE-DESIST-TYPE TO F39.
           IF BW-IS-CEASE-DESIST
              MOVE "SN N060391." TO VDU
              MOVE BW-CEASE-DESIST-DATE TO VDUNUM0
              PERFORM SCREENIO.

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

       DISPLAY-FIELDS-EXIT.
           EXIT.

      ***********************************
       VERIFY-DELETE SECTION.
      ***********************************

           PERFORM OPEN-LN4-FILE.
           PERFORM OPEN-LH1-FILE.

           PERFORM CHECK-LOAN-ACTIVITY.

      * TRYING TO DELETE A SSNO OF ALL ZEROS, AND THINKS EVERY LOAN
      * IS HIS...
           IF IO-FG = 9
              MOVE "BORROWER HAS LOANS/LOAN HISTORY" TO MESS
              PERFORM ERROR-MESSAGE.
           IF BW-LOANS NOT = 0
              MOVE "LOAN COUNT NOT = 0" TO MESS
              PERFORM ERROR-MESSAGE.
           IF BW-LIASMLN NOT = 0
              MOVE "SMALL LOAN LIABILITY NOT = 0" TO MESS
              PERFORM ERROR-MESSAGE.
           IF BW-LIAOTH NOT = 0
              MOVE "OTHER LOAN LIABILITY NOT = 0" TO MESS
              PERFORM ERROR-MESSAGE.
           IF BW-INTLYR NOT = 0
              MOVE "INTEREST LAST YEAR NOT = 0" TO MESS
              PERFORM ERROR-MESSAGE.
           IF BW-INTYTD NOT = 0
              MOVE "INTEREST YTD NOT = 0" TO MESS
              PERFORM ERROR-MESSAGE.
           IF BW-LASTPODATE NOT = 0
              MOVE "LAST PAYOFF DATE NOT = 0" TO MESS
              PERFORM ERROR-MESSAGE.
           GO TO EXIT-VERIFY-DELETE.
       ERROR-MESSAGE.
           PERFORM SEND-MESS.
           MOVE 9 TO IO-FG.
       EXIT-VERIFY-DELETE.
           IF IO-FG = 9
              MOVE "ACTIVE BORROWER, DELETE IGNORED !" TO MESS
              PERFORM SEND-MESS.
           PERFORM CLOSE-LN4-FILE.
           PERFORM CLOSE-LH1-FILE.

      **************************************
       CHECK-LOAN-ACTIVITY SECTION.
      **************************************
           
           MOVE LN-PATH-OWNBR    TO LN-OWNBR
                                    QLN4-WBEG-OWNBR
                                    QLN4-WEND-OWNBR.
           MOVE BW-SSNO          TO LN-SSNO(1)
                                    QLN4-WBEG-SSNO1
                                    QLN4-WEND-SSNO1.
           PERFORM START-LN4-FILE.
       NEXT-LN4-CHECK.
           PERFORM READ-LN4-FILE-NEXT.
           IF IO-BAD OR (LN-OWNBR NOT = LN-PATH-OWNBR)
                      OR (LN-SSNO(1) NOT = BW-SSNO)
              GO TO CHECK-LN5-ACTIVITY.

           MOVE 9 TO IO-FG.
           GO TO EXIT-CHECK-LOAN-ACTIVITY.
       CHECK-LN5-ACTIVITY.
           MOVE LN-PATH-OWNBR    TO LN-OWNBR
                                    QLN5-WBEG-OWNBR
                                    QLN5-WEND-OWNBR.
           MOVE BW-SSNO          TO LN-SSNO(2)
                                    QLN5-WBEG-SSNO2
                                    QLN5-WEND-SSNO2.
           PERFORM START-LN5-FILE.
       NEXT-LN5-CHECK.
           PERFORM READ-LN5-FILE-NEXT.
           IF IO-BAD OR (LN-OWNBR NOT = LN-PATH-OWNBR)
                      OR (LN-SSNO(2) NOT = BW-SSNO)
      *       GO TO CHECK-LN6-ACTIVITY.
              GO TO CHECK-LH1-ACTIVITY.

           MOVE 9 TO IO-FG.
           GO TO EXIT-CHECK-LOAN-ACTIVITY.

      *CHECK-LN6-ACTIVITY.

      *    MOVE LN-PATH-OWNBR    TO LN-OWNBR
      *                             QLN6-WBEG-OWNBR
      *                             QLN6-WEND-OWNBR.
      *    MOVE BW-SSNO          TO LN-SSNO(3)
      *                             QLN6-WBEG-SSNO3
      *                             QLN6-WEND-SSNO3.
      *    PERFORM START-LN6-FILE.
      *NEXT-LN6-CHECK.
      *    PERFORM READ-LN6-FILE-NEXT.
      *    IF IO-BAD OR (LN-OWNBR NOT = LN-PATH-OWNBR)
      *               OR (LN-SSNO(3) NOT = BW-SSNO)
      *       GO TO CHECK-LN7-ACTIVITY.

      *    MOVE 9 TO IO-FG.
      *    GO TO EXIT-CHECK-LOAN-ACTIVITY.
      *CHECK-LN7-ACTIVITY.
      *    MOVE LN-PATH-OWNBR    TO LN-OWNBR
      *                             QLN7-WBEG-OWNBR
      *                             QLN7-WEND-OWNBR.
      *    MOVE BW-SSNO          TO LN-SSNO(4)
      *                             QLN7-WBEG-SSNO4
      *                             QLN7-WEND-SSNO4.
      *    PERFORM START-LN7-FILE.
      *NEXT-LN7-CHECK.
      *    PERFORM READ-LN7-FILE-NEXT.
      *    IF IO-BAD OR (LN-OWNBR NOT = LN-PATH-OWNBR)
      *               OR (LN-SSNO(4) NOT = BW-SSNO)
      *       GO TO CHECK-LH1-ACTIVITY.

      *    MOVE 9 TO IO-FG.
      *    GO TO EXIT-CHECK-LOAN-ACTIVITY.

       CHECK-LH1-ACTIVITY.
           MOVE BW-OWNBR TO LH-BRNO
                            QLH1-WBEG-BRNO
                            QLH1-WEND-BRNO.
           MOVE BW-SSNO  TO LH-SSNO
                            QLH1-WBEG-SSNO
                            QLH1-WEND-SSNO.
           MOVE 0        TO LH-ACCTNO
                            QLH1-WBEG-ACCTNO.
           MOVE ALL "9"  TO QLH1-WEND-ACCTNO.

           PERFORM START-LH1-FILE.
       NEXT-LH1-CHECK.
           PERFORM READ-LH1-FILE-NEXT.
           IF (IO-FG = 9) OR (LH-BRNO NOT = LH-PATH-OWNBR)
                           OR (LH-SSNO NOT = BW-SSNO)
              MOVE 0 TO IO-FG
              GO TO EXIT-CHECK-LOAN-ACTIVITY.

           MOVE 9 TO IO-FG.
       EXIT-CHECK-LOAN-ACTIVITY.
           EXIT.

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
           MOVE ELE         TO VDUROW.
           MOVE 0           TO VDUCOL.
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

           IF ( CYSCAN-STATUS NOT = "00"   ) OR
              ( CYSCAN-KEY-CITY = SPACES )
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
      *    MOVE 10          TO CTSCAN-BEGIN-X.
      *    MOVE 4           TO CTSCAN-BEGIN-Y.

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


      ******************************************************************
       VERIFY-PASSWORD SECTION.
      ******************************************************************
           MOVE " "                          TO PASSWDW-STAT.
           MOVE "BWMAN1"                     TO PASSWDW-PGM.
           MOVE "PLEASE ENTER THE PASSWORD," TO PASSWDW-PROMPT1.
           MOVE "CL/PASSWD"                  TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH PASSWDW-BUF.
           CANCEL FORM-PROGX.

       EXIT-VERIFY-PASSWORD.
           EXIT.

      **************************************
       VERIFY-PASSWORD-MUST-EXIST SECTION.
      **************************************
           MOVE "EN"                         TO PASSWDW-STAT.
           MOVE "BWMAN1"                     TO PASSWDW-PGM.
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

      ******************************************************************
       DO-STATE-CY-EXIST SECTION.
      ******************************************************************

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

      ******************************************************************
       DO-STATE-CT-EXIST SECTION.
      ******************************************************************

      *DETERMINE IF A VALID CD-CT-REC EXISTS FOR THE BORROWERS STATE
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

       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/GPENV.CPY".
       COPY "LIBGB/DATER.CPY".
       COPY "LIBLP/LPBWCL.CPY".

      **************************************************************************
       FORM-RESET SECTION.
      **************************************************************************
           MOVE EXT-CONAME-CONAME TO BWMAN1-CONAME.
           MOVE EXT-CONAME-SYSDATE TO BWMAN1-SYSDATE.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DISPLAY BWMAN1-SCREEN UPON BWMAIN-WINDOW-HANDLE.
           MOVE BWMAN1-FORM-FIELDS TO WR-BUF.
           MOVE BWMAN1-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      **************************************************************************
       SETUP-LINK-INFO SECTION.
      **************************************************************************
           MOVE BWMAN1-HELP--FILE TO HELP-LINK-FILE.
           MOVE BWMAN1-HELP--DIR TO HELP-LINK-DIR.

      **************************************************************************
       VDU-CONVERT-FIELD SECTION.
      **************************************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBMN/BWMAN1_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      *****************************************
       MISC-PARAGRAPHS SECTION.
      *****************************************
       COPY "LIBGB/PHONEIO.CPY".
       COPY "LIBGB/GETENV.CPY".
       COPY "LIBGB/ACCESS.CPY".
       RECORD-DELETED.
           MOVE "RECORD WAS DELETED!" TO MESS.
           PERFORM SEND-MESS.
       RECORD-CHANGED.
           MOVE "CHANGES WERE MADE TO THIS RECORD!" TO MESS.
           PERFORM SEND-MESS.
       CHANGE-AGAIN.
           MOVE "PLEASE RE-INPUT YOUR CHANGES!" TO MESS.
           PERFORM SEND-MESS.
       ALREADY-CREATED.
           MOVE "RECORD HAS BEEN ALREADY CREATED!" TO MESS.
           PERFORM SEND-MESS.
       UNDERGONE-CHANGE.
           MOVE
           "THIS RECORD HAS UNDERGONE CHANGE BY ANOTHER USER,"
           TO MESS.
           PERFORM SEND-MESS.
       ACTIVE-RECORD.
           MOVE "ACTIVE CUSTOMER CANNOT BE DELETED" TO MESS.
           PERFORM SEND-MESS.
       GET-CD.
           PERFORM OPEN-CD1-FILE.
           PERFORM READ-CD1-FILE.
           MOVE IO-FG TO HOLD-IO-FG.
           PERFORM CLOSE-CD1-FILE.

      *****************************************
       IO-ROUTINE SECTION.
      *****************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-ST1-FILE.
      *    PERFORM CLOSE-LN7-FILE.
      *    PERFORM CLOSE-LN6-FILE.
           PERFORM CLOSE-LN5-FILE.
           PERFORM CLOSE-LN4-FILE.
           PERFORM CLOSE-LH1-FILE.
           PERFORM CLOSE-CD1-FILE.
           PERFORM CLOSE-BX1-FILE.
           PERFORM CLOSE-BW4-FILE.
           PERFORM CLOSE-BW3-FILE.
           PERFORM CLOSE-BW2-FILE.
           PERFORM CLOSE-BW1-FILE.
           PERFORM CLOSE-BM1-FILE.
           PERFORM CLOSE-BR-FILE.
       COPY "LIBLP/LPSTGS_SQL.CPY".
       COPY "LIBLP/LPLNGS_SQL.CPY".
       COPY "LIBLP/LPLHGS_SQL.CPY".
       COPY "LIBLP/LPCDGS_SQL.CPY".
       COPY "LIBLP/LPBXGS_SQL.CPY".
       COPY "LIBLP/LPBWGS_SQL.CPY".
       COPY "LIBLP/LPBMGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBLP/LPBX1IN.CPY".
       COPY "LIBLP/LPBW1IN.CPY".
       COPY "LIBLP/LPBW2RN.CPY".
       COPY "LIBLP/LPBW3RN.CPY".
       COPY "LIBLP/LPBW4RN.CPY".
       COPY "LIBLP/LPST1RN.CPY".
       COPY "LIBLP/LPBM1IN.CPY".
       COPY "LIBLP/LPLN4RN.CPY".
       COPY "LIBLP/LPLN5RN.CPY".
      *COPY "LIBLP/LPLN6RN.CPY".
      *COPY "LIBLP/LPLN7RN.CPY".
       COPY "LIBLP/LPLH1RN.CPY".
       COPY "LIBLP/LPCD1RN.CPY".

       COPY "LIBGB/FERRORS.CPY".

      ******************************************
       END-ROUTINE SECTION.
      ******************************************
           PERFORM CLOSE-FILES.
       EXIT-PROG.
           MOVE FORM-PATH TO FORM-PATHNAME.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY BWMAN1-SCREEN.
           MOVE BWMAIN-LINK TO FORM-LINK-AREA.
       STOP-RUN.
           EXIT PROGRAM.
