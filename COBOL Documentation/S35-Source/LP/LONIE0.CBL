       IDENTIFICATION DIVISION.
       PROGRAM-ID.      LONIE0.
       DATE-WRITTEN.    020984.
      *****************************************************
      *                 (LP)
      *  DESCRIPTION:   BORROWER MASTER FILE MAINTENANCE
      *                 (CONVERSIONS)
      * REV:
      *  BAH 060295 F10 WAS DROPPING TO $, BECAUSE THERE WAS NO
      *             MOVE FORM-PATHNAME TO FORM-PATH
      *  KEC 041697 CHANGED ACCESS TO BW-REPCODE FROM NUMERIC TO ALPHA.
      *  MJD 112497 EXPANDED BW-REC TO 480 BYTES
      *  MJD 021998 ADDED BW-SIZE (78 LEVEL FOR REC SIZE)
      *  GLF 062899 ADDED PASSWORD TO CHANGE REFERENCE DATE
      *  GLF 990909 CHANGE SSNO ENTRY TO USE NEW VDUCLASS "X" EDIT
      *   CS 991011 ADDED TEST OF BULK-CONVERSION-FG
      *  BLV 991130 FIX PROBLEM WITH NEW SSNO ENTRY & SPOUSE SSNO
      *             ENTRY
      *  BLV 991202 JOHN TWEAKED SCREEN HANDLER ENTRY & I FIXED
      *             A COUPLE OTHER THINGS
      *  JTG 000823 CHANGED AGE INPUTS TO "ERNM060...."
      *  BAH 001004 FORCE BW-REPCODE ENTRY TO "YES" OR "NO" FOR TAX PRE
      *             REGISTRATION, WORLD PR# 240
      *  CS  020220 IF BR-FORCE-VALID-CITY-COUNTY = "Y" THE CODE FILE MUST BE
      *             EXAMINED FOR "CT" & "CY" ENTRIES FOR THE STATE.  IF ANY
      *             ENTRIES EXIST FOR THE STATE, ANY SUBSEQUENT BORROWER ENTRY
      *             USING THAT STATE (BW-STATE) REQUIRES A VALID "CT" & "CY"
      *             RECORD IN THE CODE FILE
      *  CS  020312 REMOVED EXTRA PERIOD FROM SET-CY-CT-SCAN SECTION
      * BAH 030410 ADDED BW-BAD-ADR-FG AND BW-BAD-PHONE-FG TRIAD REGACC PR# 26
      * BAH 030430 ADDED SET OF BW-ADR-PHONE-CHG-DATE FOR TRIAD REGACC PR# 26
      * BAH 030728 ADDED BW-BAD-EMPPHONE-FG TRIAD REGACC PR# 36
      * BAH 031112 ENTRY OF DEFAULT SPOUSE HAD FIELDS MISALIGNED
      *            REFDATE WAS NOT SET ON ACCEPTING A SPOUSE RECORD
      * BAH 031114 ADDED GP-USE-BW-BAD-FLAGS FOR THE ENTRY OF THE ABOVE REGACC
      *             FIELDS...SUPPORT PR# 2155
      * BAH 031216 PROTECT-UNPROTECT WAS ALL COMMENTED OUT FOR SKIPPING
      *            FIELDS DURING CONVERSION ENTRY....FIXED AND TESTED
      *            FLAGS FOR WHAT HAS TO BE FORCED, JOSH
      * BAH 040218 AUTOMATICALLY SET BW-BAD-PHONE-FG FOR SPECIFIC
      *            PHONE NUMBERS, REGACC PR# 48
      * BAH 040409 CHANGE PROMPT 24 SOLICIT TO 24 CONTACT AND PROMPT 39 FROM
      *            CALL SUNDAY TO 39 OK SOLICIT FOR LENDMARK PR# 2271
      *            ALSO ADDED PASSWORDS FOR FIELD 24 AND 38, EVEN THOUGH
      *            CHANGES ARENT ALLOWED IN HERE...TO STAY CONSISTENT
      * BAH 061116 FORGOT TO DELETE BX TRAILER RECORD, LENDMARK PR# 2044
      * BAH 080401 TEST THAT AGE MUST BE GREATER THAN 18 YRS AND BIRTH-DATE
      *            CAN NOT BE TODAYS DATE, WORLD PR# 577
      * BAH 080702 ADDED BW-CELL-PHONE FIELD 8, UNITED PR# 3205
      * BAH 080912 ADDED PAGE 3, MISC PHONE CONTACTS, BWCRE2  SUPPORT PR#3226
      * BAH 090112 DONT ALLOW SAME SSNO FOR BORROWER AND SPOUSE
      *            DONT ALLOW SPOUSE SSNO IF NO SPOUSE NAME, WORLD (EAGLE PASS)
      * BAH 100216 WAS NOT GOING TO ZIP IF YOU SPACES OUT THE STATE, WORLD
      * BAH 100608 ADDED BW-BAD-CELL-FG FIELD 081, REGACC PR# 266
      *  CS 110511 ADD TEST IF GP-I-AM-REGENCY & BR-STATE = "KY", FORCE "KY"
      *            TO CD-CY-STATE & CD-CT-STATE. REGENCY WANTS TO CHARGE
      *            THE TAX ON ALL BORROWERS IN KY BRANCH REGARDLESS OF
      *            OF BW-STATE   REGENCY PR#3784
      * BAH 130819 CHANGE 38 OPT OUT FOR WORLD TO TXT OPTION, WORLD PR# 919
      * BAH 140225 CHANGE VALUES ALLOWED FOR WORLD TXT OPTION, WORLD PR# 970
      * BLM 141001 ALLOW VALUE OF "D" (DECLINED) FOR WORLD TXT OPTION 
      *            (BW-PRIVACY-OPT-OUT), WORLD PR# 1023
      * BAH 151030 REDISPLAY BW-REPCODE IF BWCRE2 CHANGED IT, HONOR PR#4898
      * BAH 151216 ALLOW "C" IN BAD PHONE FLAGS, SOAUTO PR#4912
      * KEC 2016.0324 CHANGED CDSCAN-WINDOW-BUF TO HAVE MATCHING CD1-KEY
      *              LAYOUTS (FOR EACH CODE TYPE; CD-TYPE).
      *              CDSCAN-CD1-KEY IS THE NEW (PRIMARY) WITH REDEFINES OF
      *              CDSCAN-??-KEY (NEW).
      * KEC 2016.0328 CHANGED CYSCAN WORKERS TO MATCH CD1-KEY FOR CY LAYOUT.
      *              CHANGED CTSCAN WORKERS TO MATCH CD1-KEY FOR CT LAYOUT.
      * BAH 160715 DEFAULT BW-REPCODE TO "P" FOR HONOR, IF EMAIL IS SPACES
      *            REPCODE = "P", IF NOT SPACES, IT IS "E". PR#5130
      * KEC 2016.0928 ADDED WARNING MESSAGE FOR CHANGE OF BW-BRANCH.
      * BAH 2016.1020 ADDED BW-BDATE-YYYYMMDD AND BW-SP-BDATE-YYYYMMDD,
      *               CHANGE FIELD 10 AND 37 TO NOW ENTER FULL DATE AND
      *               STORE IN THESE 2 NEW FIELDS, BUT STILL FILL IN THE
      *               OLD BW-BDATE AND BW-SP-BDATE WITH 6 DIGIT DATE
      * BAH 161024 DEFAULT SPOUSE RECORD WHEN IT AUTOMATICALLY FILLS IN ON THE
      *            FLIP TO "N", HONOR PR#5183
      * BAH 170609 ACTIVATE 8 DIGIT DATES, PD0006
      * BAH 180207 DO NOT VALIDATE SPOUSE BDATE FOR MEXICO, THEY USE IT FOR
      *            LOAN APP DATE, WORLD PR#1304
      * BAH 180426 ADDED PASSWORD 15 FOR MEXICO TO NOT ALLOW CHANGES TO
      *            AN EXISTING CENSUS AND ORGSLSMN WHICH IS WHERE THEY STORE
      *            EMPLOYEE AND PROMOTER WHO GAVE THE LOAN #1316
      * BAH 181001 GLOBAL BWFILE
      * BAH 181127 GLOBAL LHFILE
      * BAH 181210 GLOBAL LNFILE
      * BAH 190122 SPLIT CDFILE
      * BAH 07.09.2019 REMOVED GP-LR-TRAILER-TRIAD, PT0151 
      * BAH 02.10.2020 ONLY ALLOW VALID CODES FOR ORIGIN #1421
      * BLM 210924 FIX CAST ERROR IF ALPHA ENTERED IN SSNO SCAN
      * BAH 20230713 COMMENT OUT 3RD AND 4TH BORROWER TESTS, LN6 AND LN7
      * BAH 2024.1204 REPLACED CALL ON SUNDAY WITH BW-CEASE-DESIST-TYPE AND
      *               DONT ALLOW CHANGES TO FIELDS THAT ARE TIED TO IF THEY
      *               IN CEASE AND DESIST STATUS WITHOUT A PASSWORD #1686
      *                                                           S35Q-145
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
           "@(#)LP/LONIE0.CBL S35 02/20/24-14:50:42 >".
      *******************************
      *   PROGRAM WORKERS           *
      *******************************
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
       COPY "LIBLP/LP01LN.CPY".
       COPY "LIBLP/LP01LN_SQL.CPY".
       COPY "LIBLP/LPWSLN.CPY".
       COPY "LIBLP/LP01LH.CPY".
       COPY "LIBLP/LP01LH_SQL.CPY".
       COPY "LIBLP/LPWSLH.CPY".


       01  TYP             PIC 9       VALUE 0.
       01  ONE             PIC S9.
       01  ELE             PIC 9(2).
       01  SPOUSE-FLAG     PIC X VALUE "N".
       01  SPOUSE-EXISTS   PIC X VALUE "N".
       01  JUSTIFY-CONAME  PIC X(40).

       01  MSEL-LIST       PIC X(5) VALUE "MSEL.".
       01  ACTION.
           03  CHKEY       PIC X(4).
           03  CHNO        PIC 9(2).
       01  FORM-NUM1       REDEFINES ACTION.
           03  FORM-KEY1   PIC X(4).
           03  FORM-P1     PIC X.
           03  FORM-SEL1   PIC 9.
       01  IO-TYPE         PIC X(3).
       01  ALLOW-CHANGES   PIC X.
       01  HOLD-IO-FG      PIC 99.
       01  NEW-FLAG        PIC 9.
       01  HOLD-SYS-DATE   PIC 9(8) COMP-3.

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

       01  AGE-MESSAGE.
           03  FILLER                PIC X(43) VALUE
           "DATE OF BIRTH ENTERED MAKES BORROWER < 18. ".
           03  FILLER                PIC X(18) VALUE
           "PLEASE VERIFY DOB!".

      * ORIGINAL RECORD - REPLACING LEADING ON BW-REC FOR ORIG-BW-REC
       COPY "LIBLP/LP01BW.CPY" REPLACING LEADING "BW" BY "ORIG-BW".
       01  CHANGED-BW-REC     PIC X(BW-SIZE).

       01  FIRST-SCAN      PIC 9       VALUE 0.

       01  DEFAULT-SPOUSE  PIC X       VALUE "N".
       01  SPOUSE-RECORD   PIC X       VALUE "N".
       01  SAVE-SPSSNO     PIC 9(9).
       01  TEMP-SPSSNO     PIC 9(9).
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

       01  HOLD-BDATE        PIC 9(8)  COMP-3.
       01  HOLD-SP-BDATE     PIC 9(8)  COMP-3.

       01  NAME            PIC X(16).
       01  HOLD-KEY        PIC 9(9).

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

      **********************************
      *   SCREEN DISPLAY BUFFER        *
      **********************************
       01  DISPLAY-BUF     VALUE " ".
           03  F01         PIC X(16).
           03  F02         PIC X(16).
           03  F03         PIC X(24).
           03  F04         PIC X(24).
           03  F05         PIC X(15).
           03  F06         PIC XX.
           03  F061        PIC X(10).
           03  F062        PIC X.
           03  F07         PIC X(12).
           03  F071        PIC X.
           03  F08         PIC X(12).
           03  F081        PIC X.
           03  F09         PIC 99/99/99.
           03  F10         PIC 99/99/9999.
           03  F11         PIC X(4).
           03  F12         PIC ZZ.
           03  F13         PIC X(4).
           03  F14         PIC X(4).
           03  F141        PIC XX.
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
      * CEASE AND DESIST TYPE
           03  F39         PIC X.

       01  DISPLAY-LIST.
           03  FILLER      PIC X(56) VALUE
           "010 020 030 040 050 060 061 062 070 071 080 081 090 100 ".
           03  FILLER      PIC X(48) VALUE
           "110 120 130 140 141 150 160 161 170 180 190 200 ".
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
      *10. AGE OR BIRTH-DATE BDATE
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
      *21  CITY TAX CODE
           03  FILLER      PIC X(8) VALUE "E  A030 ".
      *22. ORIGINAL SALESMAN
           03  FILLER      PIC X(8) VALUE "EN N040 ".
      *23. REP CODE
      *--->03  FILLER      PIC X(8) VALUE "EN N040 ".
           03  FILLER      PIC X(8) VALUE "E  A040 ".
      *24. SOLICIT CODE
           03  FILLER      PIC X(8) VALUE "E  A010 ".
      *25. CONFIDENTIAL
           03  FILLER      PIC X(8) VALUE "E  A010 ".
      *26. EMPLOYER'S NAME
           03  FILLER      PIC X(8) VALUE "E  A240 ".
      *27. EMPLOYER'S ADDRESS 1
           03  FILLER      PIC X(8) VALUE "E  A240 ".
      *28. EMPLOYER'S ADDRESS 2
           03  FILLER      PIC X(8) VALUE "E  A240 ".
      *29. EMPLOYMENT POSITION
           03  FILLER      PIC X(8) VALUE "E  A240 ".
      *30. EMPLOYMENT PHONE NUMBER
           03  FILLER      PIC X(8) VALUE "E  A120 ".
      *31. YEARS OF EMPLOYMENT
           03  FILLER      PIC X(8) VALUE "EN N020 ".
      *32. NET INCOME MONTHLY
           03  FILLER      PIC X(8) VALUE "E  N062 ".
      *33. OTHER INCOME MONTHLY
           03  FILLER      PIC X(8) VALUE "E  N062 ".
      *34. SPOUSE'S LAST NAME
           03  FILLER      PIC X(8) VALUE "E  A160 ".
      *35. SPOUSE'S FIRST NAME
           03  FILLER      PIC X(8) VALUE "E  A160 ".
      *36. SPOUSE'S SOCIAL SECURITY NUMBER
           03  FILLER      PIC X(8) VALUE "ERNX090 ".
      *37. SPOUSE'S AGE/BIRTH BDATE
           03  FILLER      PIC X(8) VALUE "ERND080 ".
      *38. PRIVACY OPT OUT
           03  FILLER      PIC X(8) VALUE "E  A010 ".
      *39. CEASE AND DESIST TYPE
           03  FILLER      PIC X(8) VALUE "E  A010N".

       01  SPEC-TAB       REDEFINES SPEC-TABLE.
           03  SPEC-REC   OCCURS 39 TIMES.
               05  SPEC    PIC X(7).
               05  CHFG    PIC X.

       COPY "LIBLP/LPMNUIW.CPY".
       COPY "LIBLP/LPBAGEW.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/PASSWDW.CPY".
       01  REFDATE-SEQ  PIC 9  VALUE 1.
       78  SOLICIT-SEQ         VALUE 6.
      * THIS ONE DOES NOT ALLOW ANY CHANGE
       78  OPT-OUT-SEQ2        VALUE 7.
       78  18-22-SEQ           VALUE 15.
       78  CEASE-DESIST-SEQ    VALUE 17.

       COPY "LIBWI/PFSWINDOWSW.CPY".
       COPY "LIBWI/CTSCANW.CPY".
       COPY "LIBWI/CYSCANW.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/PHON400.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/GPENVW.CPY".
       COPY "LIBWI/CDSCANW.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBLP/LPCMNU_DEF.CPY".
       COPY "LIBLP/LONIE_DEF.CPY".
       COPY "LIBWI/CEASED_DEF.CPY".
       COPY "LIBLP/LPCMNU_WKS.CPY".
       COPY "LIBLP/LONIE_WKS.CPY".
       COPY "LIBWI/CEASED_WKS.CPY".

       COPY "LIBGB/GETFMW.CPY".
       COPY "LIBLP/LPLONIW.CPY".

       SCREEN SECTION.
       COPY "LIBLP/LPCMNU_SCN.CPY".
       COPY "LIBLP/LONIE_SCN.CPY".
       COPY "LIBWI/CEASED_SCN.CPY".

       PROCEDURE DIVISION
           USING FORM-PATHNAME EXIT-PATHNAME PROG-BUF.

      /***********************************
      *   P R O G R A M   C O N T R O L  *
      ************************************
       MAIN-PROGRAM SECTION.

           PERFORM SQL-CONNECT.

           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "LONIE" TO VDU-FORM-NAME.

           MOVE BRANCH TO BR-NO.
           PERFORM OPEN-BR-FILE.
           PERFORM READ-BR-FILE.
           PERFORM CLOSE-BR-FILE.

           PERFORM GET-GPENV.

           MOVE EXT-CONAME-SYSDATE TO ALP-DATE.
           PERFORM CONVERT-ALPDATE-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD TO HOLD-SYS-DATE.

       MAIN-MODULE.
      D    STOP MESS.
           PERFORM FORM-RESET.
           PERFORM DISPLAY-BULK-CONVERSION.

           PERFORM REC-BW-MODULE.
           IF IO-TYPE = "END"
              MOVE " " TO SAVEKEY
              GO TO END-ROUTINE.
           IF IO-TYPE = "CLR"
             GO TO MAIN-MODULE.

           IF BW-IS-CEASE-DESIST
              PERFORM CEASE-POP-UP.

           PERFORM ENTRY-MODULE.
           IF IO-TYPE = "RWR"
              PERFORM WRITE-MODULE.
           IF IO-TYPE = "DEL"
              PERFORM DELETE-MODULE.
           IF FORM-SEL1 NOT = 0
              IF FORM-P1 = "P"
                 MOVE BW-SSNO TO SAVEKEY
                 MOVE MENU-PROG-FORM(1) TO NEW-PROG-FORM
                 MOVE 0 TO NEW-PROG-SUB
                 MOVE "2" TO ERRCD
                 GO TO END-ROUTINE.
           GO TO MAIN-MODULE.

      *******************************
       REC-BW-MODULE SECTION.
      *******************************
       REC-BW-MODULE-NO.
           MOVE " " TO IO-TYPE.
           IF SAVEKEY NOT = " "
              MOVE SAVEKEY TO BW-SSNO NUM-BW HOLD-KEY
              PERFORM NUMBW-TO-ALPBW
              MOVE SPACES TO SAVEKEY
              MOVE "S  A110BWNO." TO VDU
              MOVE ALP-BW TO VDUBUF
              PERFORM SCREENIO
              GO TO READ-BWFILE.
           MOVE 0 TO NEW-FLAG.
           IF DEFAULT-SPOUSE = "Y"
              MOVE SAVE-SPSSNO TO BW-SSNO NUM-BW HOLD-KEY
              PERFORM NUMBW-TO-ALPBW
              MOVE "S  A110BWNO." TO VDU
              MOVE ALP-BW TO VDUBUF
              PERFORM SCREENIO
              GO TO READ-BWFILE.

       REC-BW-SSNO.
           MOVE "F7-MENU, F6-SCAN" TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "ERNX090BWNO." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>"
              DESTROY ALL CONTROLS
              PERFORM MENU-SELECTION
              IF MENU-FG = 99
                 GO TO REC-BW-MODULE-NO
              ELSE
                 MOVE "END" TO IO-TYPE
                 GO TO END-REC-BW-MODULE.
           IF VDUSTATUS = "1<"
              PERFORM ENTER-SCANKEY
              GO TO END-REC-BW-MODULE.
           IF VDUSTATUS NOT = "00"
              GO TO REC-BW-MODULE-NO.
           MOVE VDUNUM0 TO BW-SSNO HOLD-KEY.
           IF BW-SSNO = 0
              GO TO REC-BW-SSNO.
       READ-BWFILE.
           PERFORM OPEN-BW1-FILE.
           PERFORM READ-BW1-FILE.
           PERFORM CLOSE-BW1-FILE.
           IF IO-FG = 0
              MOVE BW-REC TO ORIG-BW-REC
              PERFORM DISPLAY-FIELDS
              MOVE 0 TO NEW-FLAG
           ELSE
              PERFORM NEW-RECORD-SETUP.

           IF DEFAULT-SPOUSE = "Y"
              MOVE "N" TO DEFAULT-SPOUSE
              MOVE "Y" TO SPOUSE-RECORD
              MOVE SAVE-SPLNAME  TO BW-LNAME
              MOVE SAVE-SPFNAME  TO BW-FNAME
              MOVE SAVE-ADDR1    TO BW-ADR1
              MOVE SAVE-ADDR2    TO BW-ADR2
              MOVE SAVE-CITY     TO BW-CITY
              MOVE SAVE-STATE    TO BW-STATE
              MOVE SAVE-ZIP      TO BW-ZIP
              MOVE SAVE-PHONE    TO VDUBUF
              PERFORM ENTER-PHONE
              MOVE TEL-BUF       TO BW-PHONE
              MOVE SAVE-REFDATE TO ALP-DATE
              PERFORM CONVERT-ALPDATE-TO-YYYYMMDD
              MOVE DATE-YYYYMMDD TO BW-REFDATE
              MOVE HOLD-SP-BDATE TO BW-BDATE
              MOVE SAVE-LNAME    TO BW-SPLNAME
              MOVE SAVE-FNAME    TO BW-SPFNAME
              MOVE SAVE-SSNO     TO ALP-BW
              PERFORM ALPBW-TO-NUMBW
              MOVE NUM-BW        TO BW-SPSSNO
              MOVE HOLD-BDATE TO BW-SP-BDATE
              MOVE "N"           TO BW-SOLICIT
              MOVE SAVE-BUF TO WR-BUF
              MOVE SAVE-LIST TO WR-LIST
              PERFORM CALL-WRFORM.


       END-REC-BW-MODULE.
           EXIT.

      *    ************************************************
      *    *      ENTER SORTKEY ROUTINES
      *    *      TYPE = 1 - BORROWER SOCIAL SEC. #
      *    *             2 - BORROWER NAME
      *    *             3 - SPOUSE SOCIAL SEC. #
      *    *             4 - SPOUSE NAME
      *    ************************************************
       ENTER-SCANKEY SECTION.
           PERFORM OPEN-BW1-FILE.

           MOVE BW-PATH-OWNBR TO BW-OWNBR.

       ENTER-TYPE.

           MOVE "1-SS#,2-NAME,3-SP SS#,4-SP NAME,F6-SCAN:" TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "EN N010TYPE." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10"
             MOVE "CLR" TO IO-TYPE
             GO TO CLOSE-SCANKEY.
           IF VDUSTATUS = "1<"
              MOVE "SN N010TYPE." TO VDU
              MOVE 1 TO TYP VDUNUM0
              PERFORM SCREENIO
              GO TO READ-FILES-NEXT.
           IF VDUSTATUS NOT = "00"
              GO TO ENTER-TYPE.
           MOVE VDUNUM0 TO TYP.
           IF TYP < 1 OR TYP > 4
              GO TO ENTER-TYPE.

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
                    MOVE SPACES TO BW-SPNAME
                    PERFORM START-BW4-FILE.
       ENTER-KEY.
           IF TYP = 2 OR 4
              MOVE "ENTER NAME (L,F), F6-SCAN, <ENTER>-ACCEPT"
                                                             TO MESS
           ELSE
              MOVE "ENTER SOC.SEC.NO, F6-SCAN, <ENTER>-ACCEPT"
                                                             TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "E RA240SORTKEY." TO VDU
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
                 MOVE BW-REC TO ORIG-BW-REC
                 GO TO CLOSE-SCANKEY
              ELSE
                 GO TO ENTER-KEY.

           IF TYP = 1 OR TYP = 3
              INSPECT VDUBUF REPLACING ALL " " BY "0"
              MOVE VDUBUF TO TEST-SSNO
              IF TEST-SSNO NOT NUMERIC
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
           IF TYP = 1
              PERFORM READ-BW1-FILE-NEXT
           ELSE
              IF TYP = 2
                 PERFORM READ-BW2-FILE-NEXT
              ELSE
                 IF TYP = 3
                    PERFORM READ-BW3-FILE-NEXT
                 ELSE
                    IF TYP = 4
                       PERFORM READ-BW4-FILE-NEXT.
      *    UNLOCK BW-FILE.
           IF (IO-FG = 9) OR (BW-OWNBR NOT = BW-PATH-OWNBR)
              MOVE "CLR" TO IO-TYPE
              GO TO CLOSE-SCANKEY.

           MOVE "S  A110BWNO." TO VDU.
           MOVE BW-SSNO TO NUM-BW HOLD-KEY.
           PERFORM NUMBW-TO-ALPBW.
           MOVE ALP-BW TO VDUBUF.
           PERFORM SCREENIO.

           PERFORM DISPLAY-FIELDS.

           MOVE 1 TO FIRST-SCAN.
           GO TO ENTER-KEY.

       CLOSE-SCANKEY.
           PERFORM CLOSE-BW1-FILE.
       END-SCANKEY.
           MOVE 0 TO FIRST-SCAN.
           MOVE "S  A010TYPE." TO VDU.
           PERFORM SCREENIO.
       EXIT-ENTER-SCANKEY.
           EXIT.

      ***********************************
       ARRANGE-BW-KEY SECTION.
      ***********************************
           MOVE 1 TO VDU-I VDU-J.
           MOVE " " TO BW-ALP-TABLE.
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

      *    ************************************************
      *    *   ENTRY-MODULE
      *    *   NEW-FLAG = 0 - MODIFY EXISTING RECORD
      *    *              1 - INITIAL NEW RECORD ENTRY
      *    *              2 - MODIFY NEW RECORD
      *    ************************************************
       ENTRY-MODULE SECTION.
       BEGIN-ENTRY.
           MOVE " " TO MESS.
           PERFORM SEND-LEGEND.
           MOVE " " TO IO-TYPE.
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
           MOVE " " TO STAT.
           PERFORM PROTECT-UNPROTECT.

           MOVE "<RET>-ACCEPT, DELETE, FIELD #, F1-CLEAR" TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "R RA060SEL." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF VDUSTATUS NOT = "00"
              GO TO MODIFY-ROUTINE.
           MOVE VDUBUF TO ACTION.
           IF ACTION = " "
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
           IF FORM-KEY1 = " " AND FORM-P1 = "P" AND FORM-SEL1 NUMERIC
              IF FORM-SEL1 < 0 OR = 1
                 GO TO MODIFY-ROUTINE
              ELSE
                 IF FORM-SEL1 = 2
                    MOVE "RWR" TO IO-TYPE
                    GO TO EXIT-ENTRY-MODULE
                 ELSE
                 IF FORM-SEL1 = 3
                    PERFORM CALL-BWCRE2
                    GO TO MODIFY-ROUTINE
                 ELSE
                    GO TO MODIFY-ROUTINE.
           INSPECT ACTION REPLACING ALL " " BY "0".
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


      * CHANGE SOLICIT FLAG ON AN EXISTING BORROWER
           IF ELE = 24
              IF NEW-FLAG = 0
                 MOVE SOLICIT-SEQ TO PASSWDW-SEQ
                 MOVE "TO CHANGE CONTACT/SOLICIT FLAG"
                                              TO PASSWDW-PROMPT2
                 PERFORM VERIFY-PASSWORD
                 IF PASSWDW-STAT NOT = "OK"
                    GO TO MODIFY-ROUTINE.

      * CHANGE PRIVACY OPT OUT FLAG ON AN EXISTING BORROWER
           IF ELE = 38
              IF NEW-FLAG = 0
                 MOVE OPT-OUT-SEQ2 TO PASSWDW-SEQ
                 MOVE "TO CHANGE PRIVACY OPTOUT FLAG"
                                             TO PASSWDW-PROMPT2
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
           IF ELE = 1
              IF SPOUSE-RECORD = "Y"
                 MOVE "F3-ACCEPT SPOUSE RECORD" TO MESS
                 PERFORM SEND-LEGEND.
           IF ELE = 2
              MOVE "N" TO SPOUSE-RECORD.
           IF CHKEY NOT = ZEROS
              IF CHFG (ELE) = " "
                 IF NEW-FLAG NOT = 1
                    GO TO MODIFY-ROUTINE.
           IF CHFG (ELE) = "N"
              IF NEW-FLAG = 1
                 GO TO CREATE-ROUTINE
              ELSE
                 GO TO MODIFY-ROUTINE.
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
           IF F6-KEY AND ELE = 11
              MOVE "OR" TO CDSCAN-TYPE
              PERFORM CODE-SCAN
              IF IO-GOOD
                 MOVE 1 TO ONE
                 GO TO GOTO-FIELD
              ELSE
                 GO TO ENTER-ELE.
           IF F6-KEY AND ELE = 13
              MOVE "BW" TO CDSCAN-TYPE
              PERFORM CODE-SCAN
              IF IO-GOOD
                 MOVE 1 TO ONE
                 GO TO GOTO-FIELD
              ELSE
                 GO TO ENTER-ELE.
           IF F6-KEY AND ELE = 21
              IF ALLOW-CITY-SCAN-FG = "Y"
                 PERFORM CYSCAN-SCAN
                 IF IO-GOOD
                    MOVE 1 TO ONE
                    GO TO GOTO-FIELD
                 ELSE
                    GO TO ENTER-ELE.
           IF F6-KEY AND ELE = 20
              IF ALLOW-CITY-SCAN-FG = "Y"
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
       TEST-F3.
           IF VDUSTATUS = "16"
              IF SPOUSE-RECORD = "Y"
                 MOVE "N" TO SPOUSE-RECORD
                 MOVE "S  A000010." TO VDU
                 MOVE BW-LNAME TO VDUBUF
                 PERFORM SCREENIO
                 MOVE 2 TO NEW-FLAG
                 GO TO MODIFY-ROUTINE.
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

      **********************************
      *      SET RECORD ELEMENTS       *
      **********************************
       F-01.
           MOVE VDUBUF TO BW-LNAME.
           GO TO CREATE-ROUTINE.
       F-02.
           MOVE VDUBUF TO BW-FNAME.
           GO TO CREATE-ROUTINE.
       F-03.
           MOVE VDUBUF TO BW-ADR1.
           GO TO CREATE-ROUTINE.
       F-04.
           MOVE VDUBUF TO BW-ADR2.
           GO TO CREATE-ROUTINE.
       F-05.
           MOVE VDUBUF TO BW-CITY.
           GO TO CREATE-ROUTINE.

       F-06.
           IF VDUBUF = " " AND BW-ADR1 = " " AND BW-ADR2 = " "
              MOVE VDUBUF TO BW-STATE
              IF BR-FORCE-VALID-CITY-COUNTY = "Y"
                 MOVE
                 " PER BRANCH RECORD, A VALID STATE ENTRY IS REQUIRED"
                                                              TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-ELE
              ELSE
                 GO TO F-061.
           PERFORM OPEN-ST1-FILE.
           MOVE VDUBUF TO ST-CODE
                          BW-STATE.
           PERFORM READ-ST1-FILE.
           IF IO-BAD
              GO TO ENTER-ELE.

           PERFORM CLOSE-ST1-FILE.

           PERFORM SET-CY-CT-SCAN.

           IF ALLOW-CITY-SCAN-FG = "Y"
              MOVE SPACES TO CHFG(20)
           ELSE
              MOVE "N"    TO CHFG(20).

           IF ALLOW-COUNTY-SCAN-FG = "Y"
              MOVE SPACES TO CHFG(21)
           ELSE
              MOVE "N"     TO CHFG(21).


       F-061.
           MOVE "E  A100061." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO F-061.
           MOVE VDUBUF TO BW-ZIP.
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
             MOVE EXT-CONAME-SYSDATE TO VDUBUF
             PERFORM SCREENIO
             MOVE HOLD-SYS-DATE TO BW-REFDATE
           ELSE
             MOVE VDU-DATE-YYYYMMDD TO BW-REFDATE.
           GO TO CREATE-ROUTINE.

       F-10.
           MOVE VDU-DATE-YYYYMMDD TO BW-BDATE.

           IF BW-BDATE = HOLD-SYS-DATE
              MOVE "ENTER BIRTH-DATE, NOT TODAYS DATE" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.

           IF BW-BDATE NOT = 0
              MOVE BW-BDATE TO NUM-DATE
              PERFORM TSTDAT
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
           MOVE "ENNN040161." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO F-161.
           MOVE VDUNUM0 TO BW-CREDIT-SCORE.
           IF GP-BW-CREDIT-SCORE = "Y"
              IF BW-CREDIT-SCORE NOT > 0
                 MOVE "MUST ENTER CREDIT SCORE" TO MESS
                 PERFORM SEND-MESS
                 GO TO F-161.
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
           IF ALLOW-COUNTY-SCAN-FG NOT = "Y"
              GO TO CREATE-ROUTINE.

           MOVE BW-COUNTY TO CD-CT-COUNTY.
           PERFORM DO-STATE-CT-EXIST.

           IF (DO-STATE-CT-EXIST-FG NOT = "Y") OR
                           (CD-CT-COUNTY NOT = BW-COUNTY)
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
           IF TEL-BUFX NOT NUMERIC
              GO TO ENTER-ELE.
           MOVE TEL-BUF TO BW-EMPPHONE.

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
           MOVE VDUNUM0 TO BW-SSNO TEMP-SPSSNO.
           PERFORM READ-BW1-FILE.
           IF IO-FG = 0
              MOVE "Y" TO SPOUSE-EXISTS
              MOVE "SSNO ALREADY EXISTS UNDER NAME OF" TO MESS
              PERFORM SEND-MESS
              MOVE SPACES TO MESS
              STRING BW-FNAME," ",BW-LNAME,
                                       DELIMITED BY "   " INTO MESS
              PERFORM SEND-MESS.

           PERFORM CLOSE-BW1-FILE.
           MOVE CHANGED-BW-REC TO BW-REC.
           MOVE TEMP-SPSSNO TO BW-SPSSNO.
           GO TO CREATE-ROUTINE.
       F-37.
           MOVE VDU-DATE-YYYYMMDD TO BW-SP-BDATE.

      * NO DATE VALIDATION FOR MEXICO, THEY USE SPAGE FOR APP DATE
           IF BR-STATE = "MX"
              GO TO CREATE-ROUTINE.

           IF BW-SP-BDATE = HOLD-SYS-DATE
              MOVE "ENTER BIRTH-DATE, NOT TODAYS DATE" TO MESS
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
                                ALLOW-CHANGES
                                EXIT-PATHNAME.
           CANCEL FORM-PROGX.


      *******************************
       WRITE-MODULE SECTION.
      *******************************
           PERFORM OPEN-BW1-FILE.
           IF NEW-FLAG NOT = 0
              GO TO WRITE-RECORD.

           MOVE BW-REC TO CHANGED-BW-REC.
           IF ORIG-BW-REC = CHANGED-BW-REC
              GO TO END-WRITE-MODULE.

           PERFORM READ-BW1-FILE.
           IF IO-FG NOT = 0
              PERFORM UNDERGONE-CHANGE
              PERFORM RECORD-DELETED
              GO TO END-WRITE-MODULE.
           IF BW-REC NOT = ORIG-BW-REC
              PERFORM UNDERGONE-CHANGE
              PERFORM CHANGE-AGAIN
              GO TO END-WRITE-MODULE.
           MOVE CHANGED-BW-REC TO BW-REC.

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

           IF BW-SPSSNO = 0 OR
              SPOUSE-EXISTS = "Y" OR
                 SPOUSE-FLAG = "Y"
              MOVE "N" TO DEFAULT-SPOUSE
              MOVE "N" TO SPOUSE-EXISTS
              MOVE "N" TO SPOUSE-FLAG
              GO TO END-DEFAULT-BW1.

           MOVE "Y" TO DEFAULT-SPOUSE.
           PERFORM SAVE-BW-FIELDS.

           GO TO END-DEFAULT-BW1.

       SAVE-BW-FIELDS.
           MOVE "Y" TO SPOUSE-FLAG.
           MOVE BW-SPSSNO   TO SAVE-SPSSNO.
           MOVE BW-SPLNAME  TO SAVE-SPLNAME.
           MOVE BW-SPFNAME  TO SAVE-SPFNAME.
           MOVE BW-SP-BDATE TO HOLD-SP-BDATE DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYYYY.
           MOVE DATE-MMDDYYYY TO SAVE-SP-BDATE.
           MOVE BW-ADR1     TO SAVE-ADDR1.
           MOVE BW-ADR2     TO SAVE-ADDR2.
           MOVE BW-CITY     TO SAVE-CITY.
           MOVE BW-STATE    TO SAVE-STATE.
           MOVE BW-ZIP      TO SAVE-ZIP.
           MOVE BW-PHONE TO TEL-BUF.
           PERFORM SEND-PHONE.
           MOVE "-" TO TEL-H1 TEL-H2.
           MOVE TEL-EDIT    TO SAVE-PHONE.
           MOVE BW-REFDATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO SAVE-REFDATE.
           MOVE BW-LNAME    TO SAVE-LNAME.
           MOVE BW-FNAME    TO SAVE-FNAME.
           MOVE BW-SSNO     TO NUM-BW.
           PERFORM NUMBW-TO-ALPBW.
           MOVE ALP-BW      TO SAVE-SSNO.
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

      ***********************************
       DELETE-MODULE SECTION.
      ***********************************
           PERFORM OPEN-BW1-FILE.
           IF NEW-FLAG NOT = 0
              GO TO END-DELETE-MODULE.

           PERFORM READ-BW1-FILE.
           IF IO-FG NOT = 0
              PERFORM UNDERGONE-CHANGE
              PERFORM RECORD-DELETED
              GO TO END-DELETE-MODULE.
           IF BW-REC NOT = ORIG-BW-REC
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

      ********************************
       NEW-RECORD-SETUP SECTION.
      ********************************
           MOVE 1 TO NEW-FLAG.
           MOVE "S  A000NEW." TO VDU.
           MOVE "*** NEW RECORD ***" TO VDUBUF.
           PERFORM SCREENIO.

           PERFORM CLEAR-BW-FILE.

           MOVE BW-PATH-OWNBR TO BW-OWNBR.
           MOVE HOLD-KEY      TO BW-SSNO.
           MOVE "SN N040190." TO VDU.
           MOVE BRANCH TO BW-ORGBR VDUNUM0.
           PERFORM SCREENIO.

           MOVE "S  A020060." TO VDU.
           MOVE BR-STATE TO BW-STATE VDUBUF.
           PERFORM SCREENIO.

           MOVE "Y" TO BW-SOLICIT.
           MOVE "N" TO STAT BW-CONFIDENTIAL BW-BAD-ADR-FG
                            BW-BAD-PHONE-FG.
           PERFORM PROTECT-UNPROTECT.

           MOVE "S  A040230." TO VDU.
           MOVE "NO" TO VDUBUF BW-REPCODE.
           PERFORM SCREENIO.

           MOVE 0 TO ELE.

      ********************************
       DISPLAY-FIELDS SECTION.
      ********************************
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
              MOVE BW-PHONE     TO TEL-BUF
              PERFORM SEND-PHONE
              MOVE "-"          TO TEL-H1 TEL-H2
              MOVE TEL-EDIT     TO F07.
           MOVE BW-BAD-PHONE-FG TO F071.
           IF BW-CELL-PHONE NOT = 0 AND
                             NOT = 0202020202 AND
                              NOT = 2020202020
              MOVE BW-CELL-PHONE TO TEL-BUF
              PERFORM SEND-PHONE
              MOVE "-" TO TEL-H1 TEL-H2
              MOVE TEL-EDIT     TO F08.
           MOVE BW-BAD-CELL-FG  TO F081.
           MOVE BW-REFDATE      TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY     TO F09.
           MOVE BW-BDATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYYYY.
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
              MOVE BW-EMPPHONE  TO TEL-BUF
              PERFORM SEND-PHONE
              MOVE "-"          TO TEL-H1 TEL-H2
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
           IF BW-SPSSNO > 0
               MOVE BW-SPSSNO   TO F36
               INSPECT F36 REPLACING ALL "/" BY "-".
           MOVE BW-SP-BDATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYYYY.
           MOVE DATE-MMDDYYYY      TO F37.
           MOVE BW-PRIVACY-OPT-OUT TO F38.
           MOVE BW-CEASE-DESIST-TYPE TO F39.

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
           MOVE "LONIE" TO VDU-FORM-NAME.

      ***********************************
       VERIFY-DELETE SECTION.
      ***********************************
           PERFORM OPEN-LN4-FILE.
           PERFORM OPEN-LH1-FILE.

           PERFORM CHECK-LOAN-ACTIVITY.
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
           IF (BW-LASTPODATE NOT = 0)
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

      ****************************
       CHECK-LOAN-ACTIVITY SECTION.
      ****************************

           MOVE LN-PATH-OWNBR TO LN-OWNBR
                                 QLN4-WBEG-OWNBR
                                 QLN4-WEND-OWNBR.
           MOVE BW-SSNO       TO LN-SSNO(1)
                                 QLN4-WBEG-SSNO1
                                 QLN4-WEND-SSNO1.
           PERFORM START-LN4-FILE.
       NEXT-LN4-CHECK.
           PERFORM READ-LN4-FILE-NEXT.
           IF IO-BAD OR (LN-OWNBR NOT = LN-PATH-OWNBR)
                      OR  (LN-SSNO(1) NOT = BW-SSNO)
              GO TO CHECK-LN5-ACTIVITY.

           MOVE 9 TO IO-FG.
           GO TO EXIT-CHECK-LOAN-ACTIVITY.

       CHECK-LN5-ACTIVITY.
           MOVE LN-PATH-OWNBR TO LN-OWNBR
                                 QLN5-WBEG-OWNBR
                                 QLN5-WEND-OWNBR.
           MOVE BW-SSNO       TO LN-SSNO(2)
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
      *    MOVE LN-PATH-OWNBR TO LN-OWNBR
      *                          QLN6-WBEG-OWNBR
      *                          QLN6-WEND-OWNBR.
      *    MOVE BW-SSNO       TO LN-SSNO(3)
      *                          QLN6-WBEG-SSNO3
      *                          QLN6-WEND-SSNO3.
      *    PERFORM START-LN6-FILE.
      *NEXT-LN6-CHECK.
      *    PERFORM READ-LN6-FILE-NEXT.
      *    IF IO-BAD OR (LN-OWNBR NOT = LN-PATH-OWNBR)
      *               OR (LN-SSNO(3) NOT = BW-SSNO)
      *       GO TO CHECK-LN7-ACTIVITY.

      *    MOVE 9 TO IO-FG.
      *    GO TO EXIT-CHECK-LOAN-ACTIVITY.

      *CHECK-LN7-ACTIVITY.
      *    MOVE LN-PATH-OWNBR TO LN-OWNBR
      *                          QLN7-WBEG-OWNBR
      *                          QLN7-WEND-OWNBR.
      *    MOVE BW-SSNO       TO LN-SSNO(4)
      *                          QLN7-WBEG-SSNO4
      *                          QLN7-WEND-SSNO4.
      *    PERFORM START-LN7-FILE.
      *NEXT-LN7-CHECK.
      *    PERFORM READ-LN7-FILE-NEXT.
      *    IF IO-BAD OR (LN-OWNBR NOT = LN-PATH-OWNBR)
      *               OR (LN-SSNO(4) NOT = BW-SSNO)
      *       GO TO CHECK-LH1-ACTIVITY.

      *    MOVE 9 TO IO-FG.
      *    GO TO EXIT-CHECK-LOAN-ACTIVITY.

       CHECK-LH1-ACTIVITY.
           MOVE 0             TO LH-ACCTNO.
           MOVE LH-PATH-OWNBR TO LH-BRNO
                                 QLH1-WBEG-BRNO
                                 QLH1-WEND-BRNO.
           MOVE BW-SSNO       TO LH-SSNO
                                 QLH1-WBEG-SSNO
                                 QLH1-WEND-SSNO.
           MOVE 0             TO LH-ACCTNO
                                 QLH1-WBEG-ACCTNO.
           MOVE ALL "9"       TO QLH1-WEND-ACCTNO.

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

      ******************************************************************
       VERIFY-PASSWORD SECTION.
      ******************************************************************
           MOVE " "                          TO PASSWDW-STAT.
           MOVE "LONIE0"                     TO PASSWDW-PGM.
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
           MOVE "LONIE0"                     TO PASSWDW-PGM.
           MOVE "PLEASE ENTER THE PASSWORD," TO PASSWDW-PROMPT1.
           MOVE "CL/PASSWD" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH PASSWDW-BUF.
           CANCEL FORM-PROGX.

      *****************************************************************
       CODE-SCAN SECTION.
      *****************************************************************

           MOVE SPACES  TO CDSCAN-CODE.

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
           IF ( CYSCAN-STATUS NOT = "00"   ) OR
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
           MOVE "CY"           TO CD-TYPE.
           MOVE BW-STATE    TO CD-CY-STATE.
      * SETUP CD-CY-CITY BEFORE PERFORM DO-STATE-CY-EXIST
           MOVE SPACES         TO CD-CY-FILLER.
           PERFORM START-CD1-FILE.

           PERFORM READ-CD1-FILE-NEXT.
           IF IO-BAD
              GO TO DO-STATE-CY-EXIST-EXIT.

           IF (CD-TYPE = "CY") AND (CD-CY-STATE = BW-STATE)
              MOVE "Y" TO DO-STATE-CY-EXIST-FG.

       DO-STATE-CY-EXIST-EXIT.
           PERFORM CLOSE-CD1-FILE.

      ******************************************************************
       DO-STATE-CT-EXIST SECTION.
      ******************************************************************

      * DETERMINE IF A VALID CD-CT-REC EXISTS FOR THE BORROWERS STATE

           MOVE " " TO DO-STATE-CT-EXIST-FG.

           PERFORM OPEN-CD1-FILE.
           MOVE "CT"           TO CD-TYPE.
           MOVE BW-STATE    TO CD-CT-STATE.
      * SETUP CD-CT-COUNTY BEFORE PERFORM DO-STATE-CT-EXIST
           MOVE SPACES         TO CD-CT-FILLER.
           PERFORM START-CD1-FILE.

           PERFORM READ-CD1-FILE-NEXT.
           IF IO-BAD
              GO TO DO-STATE-CT-EXIST-EXIT.

           IF (CD-TYPE = "CT") AND (CD-CT-STATE = BW-STATE)
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

           MOVE BW-COUNTY TO CD-CT-COUNTY.
           PERFORM DO-STATE-CT-EXIST.

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

       COPY "LIBWI/PFSWINDOWS.CPY".
       COPY "LIBGB/GPENV.CPY".
       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBLP/LPBWCL.CPY".
       COPY "LIBLP/LPBAGE.CPY".
       COPY "LIBLP/LPMNUI.CPY".
           IF BW-SSNO NUMERIC
              IF MENU-FG NOT = 99
                 MOVE BW-SSNO TO BORROWER-KEY.

      *****************************************************
       FORM-RESET SECTION.
      *****************************************************
      * CONAME IS CENTERED IN SETENV AND DOESN'T HAVE ENOUGH ROOM IN THIS
      * FORM
           MOVE EXT-CONAME-CONAME TO JUSTIFY-CONAME.
           CALL "C$JUSTIFY" USING JUSTIFY-CONAME  "L".
           MOVE JUSTIFY-CONAME TO LONIE-CONAME.
           MOVE EXT-CONAME-SYSDATE TO LONIE-SYSDATE.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DISPLAY LONIE-SCREEN UPON TCLP-MAIN-WINDOW-HANDLE.
           MOVE LONIE-FORM-FIELDS TO WR-BUF.
           MOVE LONIE-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      *****************************************************
       SETUP-LINK-INFO SECTION.
      *****************************************************
           MOVE LONIE-HELP--FILE TO HELP-LINK-FILE.
           MOVE LONIE-HELP--DIR TO HELP-LINK-DIR.

      *****************************************************
       VDU-CONVERT-FIELD SECTION.
      *****************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBLP/LONIE_EVA.CPY".
              COPY "LIBLP/LPCMNU_EVA.CPY".
              COPY "LIBWI/CEASED_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      *********************************
       MISC-PARAGRAPHS SECTION.
      *********************************
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/PHONEIO.CPY".

       DISPLAY-BULK-CONVERSION.
      *-{CS}----
           MOVE "S  A240TITLE." TO VDU.
           IF BULK-CONVERSION-FG = "B"
              MOVE "BORROWER (BULK PURCHASE)" TO VDUBUF
           ELSE
              MOVE "BORROWER(CONVERSION)" TO VDUBUF.
           PERFORM SCREENIO.
       RECORD-DELETED.
           MOVE " RECORD WAS DELETED!" TO MESS.
           PERFORM SEND-MESS.
       RECORD-CHANGED.
           MOVE " CHANGES WERE MADE TO THIS RECORD!" TO MESS.
           PERFORM SEND-MESS.
       CHANGE-AGAIN.
           MOVE " PLEASE RE-INPUT YOUR CHANGES!" TO MESS.
           PERFORM SEND-MESS.
       ALREADY-CREATED.
           MOVE " RECORD HAS BEEN ALREADY CREATED!" TO MESS.
           PERFORM SEND-MESS.
       UNDERGONE-CHANGE.
           MOVE
           " THIS RECORD HAS UNDERGONE CHANGE BY ANOTHER USER,"
           TO MESS.
           PERFORM SEND-MESS.
       ACTIVE-RECORD.
           MOVE " ACTIVE CUSTOMER CANNOT BE DELETED" TO MESS.
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
           MOVE MESS TO WR-BUF.
           MOVE MSEL-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
       PROTECT-UNPROTECT.
           MOVE STAT TO
              CHFG(15) CHFG(16) CHFG(17) CHFG(18) CHFG(19)
              CHFG(20) CHFG(21) CHFG(22) CHFG(23) CHFG(24).
      * FORCE ENTRY OF A CREDIT SCORE
           IF GP-BW-CREDIT-SCORE = "Y"
              MOVE " " TO CHFG(16).
      * FORCE ENTRY OF VALID CITY/COUNTY CODES
           IF BR-FORCE-VALID-CITY-COUNTY = "Y"
              MOVE " " TO CHFG(20) CHFG(21).
       GET-CD.
           PERFORM OPEN-CD1-FILE.
           PERFORM READ-CD1-FILE.
           MOVE IO-FG TO HOLD-IO-FG.
           PERFORM CLOSE-CD1-FILE.

      ******************************
       IO-ROUTINE SECTION.
      ******************************
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
           PERFORM CLOSE-BR-FILE.
       COPY "LIBLP/LPSTGS_SQL.CPY".
       COPY "LIBLP/LPLNGS_SQL.CPY".
       COPY "LIBLP/LPLHGS_SQL.CPY".
       COPY "LIBLP/LPCDGS_SQL.CPY".
       COPY "LIBLP/LPBXGS_SQL.CPY".
       COPY "LIBLP/LPBWGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBLP/LPST1RN.CPY".
       COPY "LIBLP/LPBX1IN.CPY".
       COPY "LIBLP/LPBW1IN.CPY".
       COPY "LIBLP/LPBW2RN.CPY".
       COPY "LIBLP/LPBW3RN.CPY".
       COPY "LIBLP/LPBW4RN.CPY".
       COPY "LIBLP/LPCD1RN.CPY".
       COPY "LIBLP/LPLN4RN.CPY".
       COPY "LIBLP/LPLN5RN.CPY".
      *COPY "LIBLP/LPLN6RN.CPY".
      *COPY "LIBLP/LPLN7RN.CPY".
       COPY "LIBLP/LPLH1RN.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/FERRORS.CPY".

      ******************************
       END-ROUTINE SECTION.
      ******************************
           PERFORM CLOSE-FILES.
       EXIT-PROG.
           DESTROY ALL CONTROLS.
       STOP-RUN.
           EXIT PROGRAM.
