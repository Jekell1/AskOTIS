       IDENTIFICATION DIVISION.
       PROGRAM-ID.      COSIGN.
       DATE-WRITTEN.    051606.
      *****************************************************
      *                 (WI)
      *
      *  DESCRIPTION:   CREATE COSIGNER RECORD.
      *
      * CALLED FROM: LP/LONPX0 -  CHANGES MENU
      *
      * REV:
      * BAH 060516 ADD COSIGNER TO AN EXISTING LOAN, REGACC PR# 128
      * BAH 061005 WAS NOT UPDATING BW-LOANS OR LIABILITY AND ALSO WAS NOT
      *            CREATING BI RECORD, JIMR
      * BAH 080401 TEST THAT AGE MUST BE GREATER THAN 18 YRS AND BIRTHDATE
      *            CAN NOT BE TODAYS DATE, WORLD PR# 577
      * BAH 080703 ADDED BW-CELL-PHONE FIELD 8. UNITED PR# 3205
      * BAH 100608 ADDED BW-BAD-CELL-FG FIELD 081, REGACC PR# 266
      *  CS 110510 ADD TEST IF GP-I-AM-REGENCY AND BR-STATE = "KY" FORCE
      *            "KY" TO CD-CY-STATE AND CD-CT-STATE. REGENCY WANTS ALL
      *            BORROWERS IN KY BRANCH TO BE CHARGED THE INSURANCE
      *            TAX REGARDLESS OF BW-STATE REGENCY PR#3784
      * BAH 120828 PASSWORD CHANGING FIRST NAME FNAME, REGMGM PR# 4100
      * BAH 120830 ADDED SET OF BW-DEALTRACK-CHG-DATE FOR REGACC PR# 319
      * BAH 130819 CHANGE 38 OPT OUT FOR WORLD TO TXT OPTION, WORLD PR# 919
      * BAH 140225 CHANGE VALUES ALLOWED FOR WORLD TXT OPTION, WORLD PR# 970
      * BLM 141001 ALLOW VALUE OF "D" (DECLINED) FOR WORLD TXT OPTION 
      *            (BW-PRIVACY-OPT-OUT), WORLD PR# 1023
      * BAH 150903 FORCE BW-PRIVACY-OPT-OUT TO "N" IF CELL PHONE CHANGES
      *            AND WRITE A MEMO, WORLD PR# 1101
      * BAH 151030 REDISPLAY BW-REPCODE IF BWCRE2 CHANGED IT, HONOR PR#4898
      * BAH 151216 ALLOW "C" IN BAD PHONE FLAGS, SOAUTO PR#4912
      * BAH 160218 EXPANDED LM-REC ADDED LM-BRNO, PD0003
      * KEC 2016.0328 CHANGED CDSCAN-WINDOW-BUF TO HAVE MATCHING CD1-KEY
      *               LAYOUTS (FOR EACH CODE TYPE; CD-TYPE).
      *               CDSCAN-CD1-KEY IS THE NEW (PRIMARY) WITH REDEFINES OF
      *               CDSCAN-??-KEY (NEW).
      * KEC 2016.0328 CHANGED CYSCAN WORKERS TO MATCH CD1-KEY FOR CY LAYOUT.
      *               CHANGED CTSCAN WORKERS TO MATCH CD1-KEY FOR CT LAYOUT.
      * BAH 160422 ADDED BI-LN-SMALLOAN FOR DUAL TEST, REGENCY PR#5030
      * BAH 160715 DEFAULT BW-REPCODE TO "P" FOR HONOR, IF EMAIL IS SPACES
      *            REPCODE = "P", IF NOT SPACES, IT IS "E". PR#5130
      * BAH 160811 DO NOT ALLOW DUPLICATE SSNO ON LOAN, AND PASSWORD
      *            ADDING COSIGNER IN SLOT 3 OR 4, WORLD PR#1175
      * BAH 160822 MOVED BM-PHONE-DESC, BM-PHONE-NO AND BM-PHONE-EXT TO BW
      *             REMOVED HOLD-PHONE-CONTACTS
      * KEC 2016.0928 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         ADDED WARNING MESSAGE FOR CHANGE OF BW-BRANCH.
      * BAH 161024 DEFAULT SPOUSE RECORD WHEN IT AUTOMATICALLY FILLS IN ON THE
      *            FLIP TO "N", HONOR PR#5183
      * BAH 170414 ACTIVATE 8 DIGIT DATES, PD0006
      * BAH 180207 DO NOT VALIDATE SPOUSE BDATE FOR MEXICO, THEY USE IT FOR
      *            LOAN APP DATE, WORLD PR#1304
      * BAH 180426 ADDED PASSWORD 15 FOR MEXICO TO NOT ALLOW CHANGES TO
      *            AN EXISTING CENSUS AND ORGSLSMN WHICH IS WHERE THEY STORE
      *            EMPLOYEE AND PROMOTER WHO GAVE THE LOAN #1316
      * BAH 180316 ADDED BI-LN-PLDATE TO BICREA CALL LIST, WORLD #1308
      * BAH 180927 GLOBAL FILES
      * BAH 181130 GLOBAL LMFILE
      * BAH 2019.0709 REMOVED GP-LR-TRAILER-TRIAD, PT0151 
      * BAH 2020.0210 ONLY ALLOW VALID CODES FOR ORIGIN #1421
      * BAH 2021.0914 FORCE A VALID BIRTHDATE, #1513
      * BAH 2023.0321 VERIFY-ZIPCODE #1596
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
           "@(#)WI/COSIGN.CBL S35 02/22/24-09:58:19 >".
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBLP/LP01ST.CPY".
       COPY "LIBLP/LP01ST_SQL.CPY".
       COPY "LIBLP/LPWSST.CPY".
       COPY "LIBLP/LP01BW.CPY".
       COPY "LIBLP/LP01BW_SQL.CPY".
       COPY "LIBLP/LPWSBW.CPY".
       COPY "LIBLP/LP01LN.CPY".
       COPY "LIBLP/LP01LN_SQL.CPY".
       COPY "LIBLP/LPWSLN.CPY".
       COPY "LIBLP/LP01LM.CPY".
       COPY "LIBLP/LP01LM_SQL.CPY".
       COPY "LIBLP/LPWSLM.CPY".
       COPY "LIBLP/LP01BX.CPY".
       COPY "LIBLP/LP01BX_SQL.CPY".
       COPY "LIBLP/LPWSBX.CPY".
       COPY "LIBLP/LP01CD.CPY".
       COPY "LIBLP/LP01CD_SQL.CPY".
       COPY "LIBLP/LPWSCD.CPY".
       COPY "LIBLP/LP01BI.CPY".
       COPY "LIBLP/LP01BI_SQL.CPY".
       COPY "LIBLP/LPWSBI.CPY".

       01  ERRCD                       PIC X VALUE " ".

       01  COSIGN-LINES              PIC 99  VALUE 22.
       01  COSIGN-COLUMNS            PIC 99  VALUE 78.
       01  COSIGN-BEGIN-Y            PIC 99  VALUE 2.
       01  COSIGN-BEGIN-X            PIC 99  VALUE 2.
       01  HOLD-MAKERCD    PIC X.
       01  SAVE-LM-MEMO.
           03  FILLER                  PIC X(16)     VALUE
               "ADDED COSIGNER: ".
           03  MEMO-SSNO               PIC 999/99/9999 VALUE ZEROES.
           03  FILLER                  PIC X(2)     VALUE SPACES.
           03  MEMO-LNAME              PIC X(23)    VALUE SPACES.
           03  MEMO-USERID             PIC X(10)     VALUE SPACES.
       01  SAVE-LM1-KEY                              VALUE SPACES.
           03  SAVE-LM-BRNO            PIC 9(04).
           03  SAVE-LM-ACCTNO          PIC 9(08).
           03  SAVE-LM-DATE            PIC 9(08).
           03  SAVE-LM-SEQ             PIC 9(03).

       01  AGE-MESSAGE.
           03  FILLER                PIC X(43) VALUE
           "DATE OF BIRTH ENTERED MAKES BORROWER < 18. ".
           03  FILLER                PIC X(18) VALUE
           "PLEASE VERIFY DOB!".

       01  LEGEND          PIC X(70).
       01  LEGEND-NO-WALK       PIC X(35) VALUE
           "F1-CANCEL, ENTER FIELD INFORMATION:".
       01  LEGEND-NO-WALK-SCAN  PIC X(44) VALUE
           "F1-CANCEL, F6-SCAN, ENTER FIELD INFORMATION:".

       01  LEGEND-01.
           03  FILLER      PIC X(41)  VALUE
           " F1, SELECT SCAN METHOD( 1-SS#, 2-NAME, 3".
           03  FILLER      PIC X(21)  VALUE
           "-SP SS#, 4-SP NAME ):".
       01  LEGEND-02.
           03  FILLER      PIC X(37)  VALUE
           "F1-CANCEL, F2-MEMO, F3-LNS, FIELD #, ".
           03  FILLER      PIC X(22)  VALUE
           "P#,   <ENTER>-UPDATE: ".
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
       01  LOAN-LIST       PIC X(18) VALUE "SCAN TYPE SORTKEY.".
       01  LOAN-BUFFER.
           03  FILLER      PIC X(5)  VALUE "MAKER".
           03  FILLER      PIC X     VALUE ":".
           03  LOAN-NAME   PIC X(24).

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

       01  COSIGN-WINDOW-HANDLE      PIC X(10).

       01  CHAN            PIC X(4).
       01  HOLD-SYS-DATE   PIC 9(8).
       01  BRANCH          PIC 9(4)   VALUE 0.
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
           03  F10         PIC 99/99/9999.
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
      *38. PRIVACY OPT OUT FLAG
           03  FILLER      PIC X(8) VALUE "E  A010 ".
      *39. CEASE & DESIST
           03  FILLER      PIC X(8) VALUE "E  A010 ".
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

      *=================================================================
      * ORG-BW-REC FROM 'REPLACING LEADING' ON BW-REC - ORIGINAL-BW-REC
      *=================================================================
       COPY "LIBLP/LP01BW.CPY" REPLACING LEADING "BW" BY "ORG-BW".
       01  CHANGED-BW-REC     PIC X(BW-SIZE).

       COPY "LIBCL/CLMENUW_EXT.CPY".
       COPY "LIBWI/CDSCANW.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBLP/LPBAGEW.CPY".
       COPY "LIBLP/LPWKPATH.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/GPENVW.CPY".
       COPY "LIBWI/BWLNS2W.CPY".
       COPY "LIBWI/BWSCANW.CPY".
       COPY "LIBWI/MEMOSCW.CPY".
       COPY "LIBWI/CTSCANW.CPY".
       COPY "LIBWI/CYSCANW.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/PASSWDW.CPY".
       78  CRSCORE-SEQ           VALUE 0.
       78  REFDATE-SEQ           VALUE 1.
       78  CONFID-SEQ            VALUE 2.
      * THIS ONE PREVENTS REMOVING IT (OPTOUT)
       78  OPT-OUT-SEQ           VALUE 3.
       78  SMSA-SEQ              VALUE 4.
       78  LNAME-SEQ             VALUE 5.
       78  SOLICIT-SEQ           VALUE 6.
      * THIS ONE DOES NOT ALLOW ANY CHANGE
       78  OPT-OUT-SEQ2          VALUE 7.
       78  FNAME-SEQ             VALUE 12.
       78  18-22-SEQ             VALUE 15.
      * BWCRE2
       78  PHONE-SEQ             VALUE 16.
       78  CEASE-DESIST-SEQ      VALUE 17.
       78  COSIGNER-3-AND-4-SEQ  VALUE 25.


       COPY "LIBGB/PHON400.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBWI/COSIGN_DEF.CPY".
       COPY "LIBWI/CEASED_DEF.CPY".
       COPY "LIBWI/COSIGN_WKS.CPY".
       COPY "LIBWI/CEASED_WKS.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/GETFMW.CPY".

       SCREEN SECTION.
       COPY "LIBWI/COSIGN_SCN.CPY".
       COPY "LIBWI/CEASED_SCN.CPY".

       PROCEDURE DIVISION
             USING FORM-PATHNAME EXIT-PATHNAME.

      ************************************
      *   P R O G R A M   C O N T R O L  *
      ************************************
       MAIN-PROGRAM SECTION.

           PERFORM SQL-CONNECT.

       COPY "LIBGB/CHKSEC.CPY".
           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "COSIGN" TO VDU-FORM-NAME.

           MOVE " " TO VDU-WIMENU-SELECTION.

           MOVE EXT-CONAME-DAILY-PASSWORD TO CHAN.

           MOVE EXT-CONAME-USER-LOC TO BR-NO BRANCH.

           MOVE BRANCH TO BR-NO.
           PERFORM OPEN-BR-FILE.
           PERFORM READ-BR-FILE.
           PERFORM CLOSE-BR-FILE.


              DISPLAY FLOATING GRAPHICAL WINDOW
                      AT  SCREEN LINE COSIGN-BEGIN-X * VDU-WIN-L-OFFSET
                      AT  SCREEN COL  COSIGN-BEGIN-Y * VDU-WIN-C-OFFSET
                   CELL SIZE IS LABEL FONT
           COLOR IS VDU-WCOLOR-TCLP,CONTROL VALUE IS VDU-WCTRL-TCLP
                      SIZE    COSIGN-COLUMNS + VDU-WIN-W-MOD
                      LINES   COSIGN-LINES + VDU-WIN-H-MOD
                      BLANK SCREEN
                      HANDLE IS COSIGN-WINDOW-HANDLE.


           MOVE EXT-CONAME-SYSDATE TO ALP-DATE.
           PERFORM CONVERT-ALPDATE-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD TO HOLD-SYS-DATE.

           PERFORM GET-GPENV.

       MAIN-MODULE.
      D    STOP MESS.
           PERFORM FORM-RESET.

           PERFORM REC-BW-MODULE.
           IF IO-TYPE = "END"
              GO TO END-ROUTINE.
           IF IO-TYPE = "CLR"
              GO TO MAIN-MODULE.

           PERFORM ENTRY-MODULE.
           IF IO-TYPE = "RWR"
              PERFORM UPDATE-LOAN
              PERFORM WRITE-MODULE
              GO TO MAIN-MODULE.
           IF IO-TYPE = "END"
              GO TO END-ROUTINE.

           GO TO MAIN-MODULE.

      *****************************************************
       REC-BW-MODULE SECTION.
      *****************************************************
       REC-BW-MODULE-NO.
           MOVE SPACES TO IO-TYPE.
           MOVE 0 TO NEW-FLAG.

           IF DEFAULT-SPOUSE = "Y"
              MOVE SAVE-SPSSNO TO BW-SSNO NUM-BW
              MOVE "S  X110BWNO." TO VDU
              MOVE BW-SSNO TO VDU-SSNO-EDIT
              PERFORM SCREENIO
              GO TO READ-BWFILE.

           MOVE "F7-EXIT, F5 OR F6-SCAN, ENTER SOC-SEC-NO:"
                                                    TO LEGEND.
           PERFORM SEND-LEGEND.

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
              MOVE SAVE-ADDR1   TO BW-ADR1
              MOVE SAVE-ADDR2   TO BW-ADR2
              MOVE SAVE-CITY    TO BW-CITY
              MOVE SAVE-STATE   TO BW-STATE
              MOVE SAVE-ZIP     TO BW-ZIP
              MOVE SAVE-PHONE   TO VDUBUF
              PERFORM ENTER-PHONE
              MOVE TEL-BUF      TO BW-PHONE
              MOVE SAVE-REFDATE TO ALP-DATE
              PERFORM CONVERT-ALPDATE-TO-YYYYMMDD
              MOVE DATE-YYYYMMDD TO BW-REFDATE
              MOVE HOLD-SP-BDATE TO BW-BDATE
              MOVE SAVE-LNAME    TO BW-SPLNAME
              MOVE SAVE-FNAME    TO BW-SPFNAME
              MOVE SAVE-SSNO     TO ALP-BW
              PERFORM ALPBW-TO-NUMBW
              MOVE NUM-BW        TO BW-SPSSNO
              MOVE HOLD-BDATE    TO BW-SP-BDATE
              MOVE "N" TO BW-SOLICIT
              MOVE SAVE-BUF TO WR-BUF
              MOVE SAVE-LIST TO WR-LIST
              PERFORM CALL-WRFORM.

       REC-LN-ACCTNO.
           MOVE "ENNN060LOAN." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>"
              MOVE "END" TO IO-TYPE
              GO TO END-REC-BW-MODULE.
           IF VDUSTATUS = "18"
              GO TO REC-BW-SSNO.
           IF VDUSTATUS NOT = "00"
              GO TO REC-LN-ACCTNO.

           MOVE BRANCH  TO LN-OWNBR.
           MOVE VDUNUM0 TO LN-ACCTNO.
           IF LN-ACCTNO = 0
              GO TO REC-LN-ACCTNO.

           PERFORM OPEN-LN1-FILE.
           PERFORM READ-LN1-FILE.
           PERFORM CLOSE-LN1-FILE.
           IF IO-FG NOT = 0
              MOVE "LOAN DOES NOT EXIST" TO MESS
              PERFORM SEND-MESS
              GO TO REC-LN-ACCTNO.

           IF LN-SSNO(2) NOT = 0 AND LN-SSNO(3) NOT = 0 AND
                   LN-SSNO(4) NOT = 0
              MOVE "LOAN ALREADY HAS 4 SIGNERS" TO MESS
              PERFORM SEND-MESS
              GO TO REC-LN-ACCTNO.

           IF BW-SSNO = LN-SSNO(1) OR LN-SSNO(2) OR LN-SSNO(3)
                                    OR  LN-SSNO(4)
              MOVE "NO DUPLICATES ALLOWED" TO MESS
              PERFORM SEND-MESS
              GO TO REC-LN-ACCTNO.

           IF LN-CURBAL = 0
              MOVE "LOAN IS PAID OUT" TO MESS
              PERFORM SEND-MESS
              GO TO REC-LN-ACCTNO.

      * IF THIS BORROWER WILL GO IN SLOT 3 OR 4, ASK FOR PASSWORD
           IF LN-SSNO(2) NOT = 0
              MOVE COSIGNER-3-AND-4-SEQ TO PASSWDW-SEQ
              MOVE "TO ADD/CHANGE COSIGNER....:" TO PASSWDW-PROMPT2
      * PERFORM THIS PASSWORD SO IT LOOKS FOR WI/COSIGN AS PROGRAM
      * AND NOT MODBOR!
              PERFORM VERIFY-COSIGN-PASSWORD
              IF PASSWDW-STAT NOT = "OK"
                 GO TO REC-LN-ACCTNO.

           MOVE LN-SSNO(1) TO BI-SSNO.
           MOVE LN-ACCTNO TO BI-ACCTNO.
           MOVE BRANCH TO BI-OWNBR.
           PERFORM OPEN-BI1-FILE.
           PERFORM READ-BI1-FILE.
           PERFORM CLOSE-BI1-FILE.
           IF IO-FG = 0
              STRING BI-FNAME," ",BI-LNAME DELIMITED BY "   "
                                           INTO LOAN-NAME
              MOVE LOAN-BUFFER TO WR-BUF
              MOVE LOAN-LIST   TO WR-LIST
              PERFORM CALL-WRFORM.


       REC-MAKERCD.
           MOVE "E  A010MAKERCD." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "18"
              GO TO REC-LN-ACCTNO.
           IF VDUSTATUS = "1>"
              MOVE "END" TO IO-TYPE
              GO TO END-REC-BW-MODULE.
           IF VDUSTATUS NOT = "00"
              GO TO REC-MAKERCD.
           MOVE VDUBUF TO HOLD-MAKERCD.
           IF HOLD-MAKERCD NOT = "J" AND NOT = "E"
              MOVE "ENTER: 'J' FOR JOINT, OR 'E' FOR ENDORSER"
                      TO MESS PERFORM SEND-MESS
              GO TO REC-MAKERCD.


       END-REC-BW-MODULE.
           EXIT.

      *********************************************
       NEW-RECORD-SETUP SECTION.
      *********************************************
           MOVE 1 TO NEW-FLAG.
           MOVE "S  A000NEW." TO VDU.
           MOVE "*** NEW RECORD ***" TO VDUBUF.
           PERFORM SCREENIO.

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

           MOVE "S  A040230." TO VDU.
           MOVE "NO" TO VDUBUF BW-REPCODE.
           PERFORM SCREENIO.

           MOVE 0 TO ELE.

      *********************************************
       ARRANGE-BW-KEY SECTION.
      *********************************************
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

      ************************************************
      *      ENTER SORTKEY ROUTINES
      *      TYPE = 1 - BORROWER SOCIAL SEC. #
      *             2 - BORROWER NAME
      *             3 - SPOUSE SOCIAL SEC. #
      *             4 - SPOUSE NAME
      ************************************************
       BORROWER-SCAN SECTION.
      ************************************************
           MOVE 0   TO BWSCAN-NO.
           MOVE "S" TO BWSCAN-SCAN-TYPE.
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
           MOVE SCAN-LIST   TO WR-LIST.
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
           MOVE SPACES TO WR-BUF.
           MOVE SCAN-LIST TO WR-LIST.
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
           MOVE LEGEND-02 TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "R RA060SEL." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF VDUSTATUS = "12"
              MOVE "B" TO MEMOSC-ONLY
              MOVE 1             TO EXT-WI-CL-SSNO-NO
              MOVE BW-SSNO       TO EXT-WI-CL-SSNO(1)
              MOVE HOLD-SYS-DATE TO EXT-WI-CL-TRANS-DATE
              MOVE "WI/MEMOSC" TO FORM-NAM
              CALL FORM-PROGX USING FORM-PATH MEMOSC-BUF EXIT-PATHNAME
              CANCEL FORM-PROGX
              GO TO MODIFY-ROUTINE.
           IF VDUSTATUS = "16"
              MOVE "Y"           TO BWLNS2-SCAN-ONLY
              MOVE BW-SSNO       TO BWLNS2-KEY
              MOVE HOLD-SYS-DATE TO DATE-YYYYMMDD
              PERFORM CONVERT-YYYYMMDD-TO-ALPDATE
              MOVE ALP-DATE      TO BWLNS2-SYSDATE
              MOVE "WI/BWLNS2" TO FORM-NAM
              CALL FORM-PROGX USING FORM-PATH BWLNS2-BUF EXIT-PATHNAME
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
                       GO TO MODIFY-ROUTINE.

           IF ACTION = SPACES
              PERFORM VALIDATE-REQUIRED-FIELD
              IF NOT (ALLOW-BW-UPDATE-FG-OK)
                 GO TO MODIFY-ROUTINE
              ELSE
                 MOVE "RWR" TO IO-TYPE
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
                 MOVE "TO CHANGE CONFIDENTIAL FLAG"
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
              MOVE "BW"   TO CDSCAN-TYPE
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
           GO TO CREATE-ROUTINE.
       F-02.
           MOVE VDUBUF TO BW-FNAME.
           IF BW-FNAME = " "
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       F-03.
           MOVE VDUBUF TO BW-ADR1.
           GO TO CREATE-ROUTINE.
       F-04.
           MOVE VDUBUF TO BW-ADR2.
           GO TO CREATE-ROUTINE.
       F-05.
           IF VDUBUF = SPACES AND BW-ADR1 = SPACES AND BW-ADR2 = SPACES
              MOVE VDUBUF TO BW-CITY
              GO TO CREATE-ROUTINE.
           MOVE VDUBUF TO BW-CITY.
           IF BW-CITY = " "
              GO TO ENTER-ELE.
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
                 GO TO CREATE-ROUTINE.
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
           MOVE BW-CELL-PHONE TO TEST-PHONE.
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
      *    -------------------------------------------------------------
      *    FORCE PASSWORD ON MODIFY OF EXISTING RECORD ONLY
      *    -------------------------------------------------------------
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

      * THIS FLAG IS SET TO "Y" IF AN ENTRY FOR THE STATE EXISTS IN THE
      * CODE FILE SO ANY ENTRY FOR THAT STATE MUST BE VERIFIED
           IF ALLOW-COUNTY-SCAN-FG NOT = "Y"
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
           IF BW-EMPPHONE = 1111111 OR 1111111111 OR
                            5555555 OR 5555555555 OR
                            8888888 OR 8888888888 OR 0
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
       F-31A.
           MOVE "E  A090311." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO F-31A.
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
           MOVE "N" TO SPOUSE-EXISTS.
           PERFORM OPEN-BW1-FILE.
           MOVE BW-REC TO CHANGED-BW-REC.
           MOVE VDUNUM0 TO BW-SPSSNO NUM-BW.
           IF IO-FG NOT = 0
              GO TO ENTER-ELE.
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

      ************************************************
       CALL-BWCRE2 SECTION.
      ************************************************
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


      ****************************
       UPDATE-LOAN SECTION.
      ****************************
           PERFORM OPEN-LN1-FILE.
           PERFORM READ-LN1-FILE.
           IF IO-FG NOT = 0
              GO TO EXIT-UPDATE-LOAN.

           IF LN-SSNO(2) = 0
              MOVE BW-SSNO TO LN-SSNO(2)
              MOVE HOLD-MAKERCD TO LN-MAKERCD(2)
              GO TO UPDATE-REWRITE-LOAN
           ELSE
      * MAKE SURE THIS COSIGNER IS NOT ALREADY ON THE LOAN
              IF LN-SSNO(2) = BW-SSNO
                 GO TO EXIT-UPDATE-LOAN.

           IF LN-SSNO(3) = 0
              MOVE BW-SSNO TO LN-SSNO(3)
              MOVE HOLD-MAKERCD TO LN-MAKERCD(3)
              GO TO UPDATE-REWRITE-LOAN
           ELSE
              IF LN-SSNO(3) = BW-SSNO
                 GO TO EXIT-UPDATE-LOAN.

           IF LN-SSNO(4) = 0
              MOVE BW-SSNO TO LN-SSNO(4)
              MOVE HOLD-MAKERCD TO LN-MAKERCD(4)
              GO TO UPDATE-REWRITE-LOAN.

       UPDATE-REWRITE-LOAN.
           PERFORM REWRITE-LN1-FILE.
           PERFORM UPDATE-MEMO-RECORD.

       EXIT-UPDATE-LOAN.
           PERFORM CLOSE-LN1-FILE.


      *****************************
       UPDATE-MEMO-RECORD SECTION.
      *****************************

           PERFORM OPEN-LM1-FILE.
           MOVE LM-PATH-OWNBR     TO SAVE-LM-BRNO.
           MOVE LN-ACCTNO         TO SAVE-LM-ACCTNO.
           COMPUTE SAVE-LM-DATE = 99999999 - HOLD-SYS-DATE.
           MOVE 1                 TO SAVE-LM-SEQ.

       CREATE-MEMO.
           MOVE SAVE-LM1-KEY TO LM1-KEY.
           PERFORM READ-LM1-FILE.
           IF ( IO-GOOD )
              IF ( LM-SEQ < 999 )
                 ADD 1 TO SAVE-LM-SEQ
                 GO TO CREATE-MEMO
              ELSE
                 GO TO UPDATE-MEMO-RECORD-EXIT.

           MOVE SPACES       TO LM-REC.
           MOVE SAVE-LM1-KEY TO LM1-KEY.
           MOVE BW-SSNO      TO MEMO-SSNO.
           INSPECT MEMO-SSNO REPLACING ALL "/" BY "-".
           MOVE BI-LNAME     TO MEMO-LNAME.
           MOVE EXT-CONAME-USER-ID TO MEMO-USERID.
           MOVE SAVE-LM-MEMO TO LM-MEMO.

           PERFORM WRITE-LM1-FILE.


       UPDATE-MEMO-RECORD-EXIT.
           PERFORM CLOSE-LM1-FILE.

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
              GO TO NO-CHANGES-TO-BW.

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

       NO-CHANGES-TO-BW.
           ADD 1 TO BW-LOANS.
           IF LN-SMALLOAN = "Y"
              ADD LN-CURBAL TO BW-LIASMLN
           ELSE
              ADD LN-CURBAL TO BW-LIAOTH.

           PERFORM REWRITE-BW1-FILE.
           PERFORM CHECK-BI-FILE.

           MOVE "CL/CHBMMO" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH
                                BW-REC
                                ORG-BW-REC
                                BRANCH.
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

           MOVE BW-SPSSNO     TO SAVE-SPSSNO.
           MOVE BW-SPLNAME    TO SAVE-SPLNAME.
           MOVE BW-SPFNAME    TO SAVE-SPFNAME.
           MOVE BW-SP-BDATE   TO HOLD-SP-BDATE DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYYYY.
           MOVE DATE-MMDDYYYY TO SAVE-SP-BDATE.
           MOVE BW-ADR1       TO SAVE-ADDR1.
           MOVE BW-ADR2       TO SAVE-ADDR2.
           MOVE BW-CITY       TO SAVE-CITY.
           MOVE BW-STATE      TO SAVE-STATE.
           MOVE BW-ZIP        TO SAVE-ZIP.
           MOVE BW-PHONE TO TEL-BUF.
           PERFORM SEND-PHONE.
           MOVE TEL-EDIT      TO SAVE-PHONE.

           MOVE BW-REFDATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY   TO SAVE-REFDATE.

           MOVE BW-LNAME      TO SAVE-LNAME.
           MOVE BW-FNAME      TO SAVE-FNAME.
           MOVE BW-SSNO TO NUM-BW.
           PERFORM NUMBW-TO-ALPBW.
           MOVE ALP-BW        TO SAVE-SSNO.
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

           MOVE 1 TO BW-LOANS.
           IF LN-SMALLOAN = "Y"
              MOVE LN-CURBAL TO BW-LIASMLN
           ELSE
              MOVE LN-CURBAL TO BW-LIAOTH.

           PERFORM WRITE-BW1-FILE.
           IF IO-FG NOT = 0
              PERFORM ALREADY-CREATED.

           PERFORM CHECK-BI-FILE.

       END-WRITE-MODULE.
           PERFORM CLOSE-BW1-FILE.


      *********************************
       CHECK-BI-FILE SECTION.
      *********************************

      * NEED TO ADD THIS NEW COSIGNER/LOAN # COMBINATION TO THE BI FILE
      * FORCE THE SSNO IN THE LAST FIELD

           MOVE "W1"        TO STAT.
           MOVE "CL/BICREA" TO FORM-NAM
           CALL FORM-PROGX USING FORM-PATH 
                                   STAT
                                   LN-ACCTNO
                                   LN-BWTABLE
                                   LN-OWNBR
                                   LN-ORGST
                                   LN-SALEFIN
                                   LN-GIFTNET
                                   LN-ENTDATE
                                   LN-POFFDATE
                                   LN-PLCD
                                   LN-CONVERCD
                                   BW-SSNO
                                   LN-SMALLOAN
                                   LN-PLDATE.

           CANCEL FORM-PROGX.

      ****************************
       DISPLAY-FIELDS SECTION.
      ****************************

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

           IF ( CYSCAN-STATUS NOT = "00"   ) OR
              ( CYSCAN-KEY-CITY = SPACES )
              MOVE 9 TO IO-FG
              GO TO CYSCAN-SCAN-EXIT.

           MOVE "S  A030210." TO VDU.
           MOVE CYSCAN-KEY-CITY TO VDUBUF.
           PERFORM SCREENIO.

           MOVE 0 TO IO-FG
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

           IF (CTSCAN-STATUS NOT = "00") OR (CTSCAN-KEY-COUNTY = SPACE)
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
       VERIFY-COSIGN-PASSWORD SECTION.
      *******************************************************
           MOVE " " TO PASSWDW-STAT.
           MOVE "COSIGN" TO PASSWDW-PGM.
           MOVE "PLEASE ENTER THE PASSWORD," TO PASSWDW-PROMPT1.
           MOVE "CL/PASSWD" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH PASSWDW-BUF.
           CANCEL FORM-PROGX.

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
           MOVE "MODBOR"                     TO PASSWDW-PGM.
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

           MOVE BW-CITY-TAX-CD TO CD-CY-CITY.
           PERFORM DO-STATE-CY-EXIST.
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

      ****************************************************
       FORM-RESET SECTION.
      ****************************************************
           DISPLAY SPACES UPON COSIGN-WINDOW-HANDLE
                          LINE 1 COL 1 ERASE SCREEN.
           DISPLAY COSIGN-SCREEN UPON COSIGN-WINDOW-HANDLE.
           MOVE COSIGN-FORM-FIELDS TO WR-BUF.
           MOVE COSIGN-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

           MOVE "S  A180TITLE:FR." TO VDU.
           MOVE " CREATE COSIGNER" TO VDUBUF.
           PERFORM SCREENIO.
       FORM-RESET-EXIT.
           EXIT.

      ****************************************************
       SETUP-LINK-INFO SECTION.
      ****************************************************
           MOVE COSIGN-HELP--FILE TO HELP-LINK-FILE.
           MOVE COSIGN-HELP--DIR TO HELP-LINK-DIR.

      ****************************************************
       VDU-CONVERT-FIELD SECTION.
      ****************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBWI/COSIGN_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.


       COPY "LIBGB/GPENV.CPY".
       COPY "LIBGB/DATER.CPY".
       COPY "LIBLP/LPBWCL.CPY".
       COPY "LIBLP/LPBAGE.CPY".
       COPY "LIBGB/SCREEN.CPY".

      *********************************
       MISC-PARAGRAPHS SECTION.
      *********************************
       COPY "LIBGB/PHONEIO.CPY".
       COPY "LIBGB/ACCESS.CPY".
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

      *********************************
       IO-ROUTINE SECTION.
      *********************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-ST1-FILE.
           PERFORM CLOSE-LN1-FILE.
           PERFORM CLOSE-LM1-FILE.
           PERFORM CLOSE-CD1-FILE.
           PERFORM CLOSE-BX1-FILE.
           PERFORM CLOSE-BW4-FILE.
           PERFORM CLOSE-BW3-FILE.
           PERFORM CLOSE-BW2-FILE.
           PERFORM CLOSE-BW1-FILE.
           PERFORM CLOSE-BI1-FILE.
           PERFORM CLOSE-BR-FILE.
       COPY "LIBLP/LPSTGS_SQL.CPY".
       COPY "LIBLP/LPLNGS_SQL.CPY".
       COPY "LIBLP/LPLMGS_SQL.CPY".
       COPY "LIBLP/LPCDGS_SQL.CPY".
       COPY "LIBLP/LPBXGS_SQL.CPY".
       COPY "LIBLP/LPBWGS_SQL.CPY".
       COPY "LIBLP/LPBIGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBLP/LPST1RN.CPY".
       COPY "LIBLP/LPLN1IN.CPY".
       COPY "LIBLP/LPBW1IN.CPY".
       COPY "LIBLP/LPBW2RN.CPY".
       COPY "LIBLP/LPBW3RN.CPY".
       COPY "LIBLP/LPBW4RN.CPY".
       COPY "LIBLP/LPBX1IN.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBLP/LPCD1RN.CPY".
       COPY "LIBLP/LPBI1RN.CPY".
       COPY "LIBLP/LPLM1IN.CPY".

       COPY "LIBGB/FERRORS.CPY".
       END-ROUTINE.
           PERFORM CLOSE-FILES.
       EXIT-PROG.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY COSIGN-SCREEN.
           DESTROY COSIGN-WINDOW-HANDLE.
       STOP-RUN.
           EXIT PROGRAM.
