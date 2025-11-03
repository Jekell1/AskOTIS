       IDENTIFICATION DIVISION.
       PROGRAM-ID.      SPMAN3.
      *****************************************************************
      *   DESC: STATE PARAMETER MAINT - PROG 3
      * REV :
      *  021588 BAW ADDED NEW SCFRMLA - 8
      *             ALSO ALLOWED SCFRMLA 1 ON IB OR PC
      *  101988 JTG ADDED NEW SCFRMLA - 9 & 10
      *  120588 JTG ADDED NEW SCFRMLA - 11
      *  081689 JTG ADDED NEW MFFRMLA -  2 (GA)
      *                       SCFRMLA - 12 AND 13 (GA)
      *  012490 JTG OPENED UP SCFRMLA - 4 TO IB ALSO (CA)
      *  032290 SLC MODIFIED FOR ADDONS
      *  050490 JTG ALLOWED US RULE WITH SP-PYEXFRMLA - 1
      *             ADDED SP-EXFRMLA 8, PACESETTER
      *  022892 BAH REMOVED ADDON JUNK
      *         JTG ADDED SP-SCFRMLA = 17, WORLD (TN)
      *                                18, WORLD (SC)
      *  102793 BAH ADDED SP-SCFRMLA = 19, CITIZENS TOTAL-NOTE + EXTCHG
      *  050895 MJD ADDED CODE TO MAKE CHANGE FILE AFTER REWRITE
      *  062095 BAH ADDED SCFRMLA = 20, MERCURY MISSOURI
      *             % OF TOTAL-NOTE - INT - SERCHG
      *  070595 JTG ADDED SCFRMLA = 21, MERCURY (MI)
      *  120195 JFF ADDED LOAN SCAN & STATE PARAMETER SCAN
      *  042996 BAH ADDED SCFRMLA = 22, PACESETTER (MI)
      *  043096 BAH ADDED SCFRMLA = 23, NORTH CAROLINA
      *  070896 BAH ADDED SCFRMLA = 25, MERCURY 'KS'
      *  100896 JFF ADDED SCFRMLA = 26, MERCURY 'WA'
      *  121396 BAH ADDED SCFRMLA = 27, MERCURY 'CA'
      *  BAH 121696 ADDED SCFRMLA 28, ADVANCE  # 405
      *  JTG 021297 ADDED THE FOLLOWING FOR MERCURY, OH 30 1 1
      *               88  SP-PYEXSPCODE-REGZ      VALUE 0.
      *               88  SP-PYEXSPCODE-REMAIN    VALUE 1.
      *  BAH 071197 ALLOW MFEES ON IB CLASSES
      *  BAH 123097 ADDED SCFRMLA 29 FOR MULLEN, PR# 589
      *  BLV 061198 CHANGE ORIG, CHANGED RECS TO USE SP-SIZE(4513) NOT 4502
      *  BAH 110398 ADDED SCFRMLA 30 FOR CITIZENS, TN PR# 779
      *  JTG 122198 ADDED SP-SCFRMLA = 31, OHIO FLAT DOLLAR   , REGENCY #003
      *  JTG 122198 ADDED SP-SCFRMLA = 32, OHIO % PRINCIPAL   , REGENCY #004
      *  JTG 122198 ADDED SP-SCFRMLA = 33, OHIO %PRIN /FEE    , REGENCY #005
      *  JTG 122198 ADDED SP-SCFRMLA = 34, PA   %CASH ADV.    , REGENCY #013
      *  BAH 021799 ADDED SP-SCFRMLA = 35, PA   RATE PER $50, REGENCY #016
      *  BAH 062399 ADDED SP-SCFRMLA = 36, POINTS, WESTERN #879
      *  BAH 072099 ADDED SP-SCFRMLA = 37, FIXED CHG, EXCEL #894
      *  BAH 991014 ADDED SP-MFFRMLA = 3, MERCURY PR# 220 TN AUDITOR
      *  BAH 000225 ADDED SP-SCFRMLA = 38, REGENCY, PR# 1095
      *  MJD 000516 CHANGED TO ALLOW SCFRMLA 39 (SUPPORT PR#1181)
      *  BAH 000920 ADDED SP-SCFRMLA = 40, REGENCY, PR# 1314, AND TOTALLY
      *             CHANGED SCFRMLA 39, THEY NEVER USED, NOW IT WILL NOT BE
      *             POINTS
      *  MJD 000928 ADDED F2-MEMO OPTION FOR NEW SPR MEMO FILE (SMFILE)
      *  MJD 001030 ADDED SP-SCFRMLA = 41, POINTS, WESTERN #1344
      *  BAH 010518 ADDED SP-SCFRMLA = 42, (SC SMALL LOAN), SUNBELT PR#1484
      *  BAH 010607 ADDED SP-SCFRMLA = 43 FOR FCF "NC", PR# 1506
      *  BAH 010607 ADDED SP-SCFRMLA = 44 FOR FCF "VA", PR# 1498
      *  BAH 010620 ADDED SP-PYEXFRMLA = 11 FOR REGWAN, PR# 1515
      *  BAH 011228 ADDED SP-SCFRMLA = 45 FOR HCRED "NC", PR# 1695
      *  BAH 020802 ADDED SP-MFFRMLA 4 FOR REGWAN, "TN" PR# 1867
      *  JTG 021025 ALLOW GP-I-AM-WORLD TO ENTER 999.998 IN SP-ACC-RATE(1)
      *             AND IF DETECTED HERE SKIP USAGE OF ACCELERATION RATE
      *             TABLE AND FORCE LN-SMPRATE RE: LN-ACCRATE 999.998 WORLD #306
      *             --- ALLOWED ENTRY 0F 999.998 IN SP-ACC-RATE(1)
      *  JTG 030128 CREATED FORMULA 12 LIKE 2 BUT SPREADS EXTENSION CHARGE
      *             OVER PAYMENTS  88  SP-PYEXFRMLA-SAME2-SPREAD VALUE 12.
      *                                                           CITIZENS #2006
      *  BAH 030129 ADDED SP-SCFRMLA 46 FOR LENDMARK PR# 2004
      *  CS  040927 ADD CALL TO CL/SPMEMO WHICH WRITES A SPR CHANGE MEMO (TYPE A
      *  JTG 050519 ADDED NEW FORMULAS FOR MEXICO
      *             SP-MFFRMLA-05 SP-RBFRMLA2 (G) SP-ERNFRMLA (24)   WORLD #0408
      *  BAH 050830 ADDED SP-SCFRMLA 47 FOR BAY COUNTRY PR# 2582
      *  JTG 061205 ADDED SP-SCFRMLA = 48,(POINTS)FOR TN REAL ESTATE LOANS
      *                                                           REGENCY  #2880
      *  BAH 070726 ADDED SP-SCFRMLA 49 FOR REGMGM PR# 3017
      *  BAH 101229 ADDED SP-SCFRMLA 50 FOR WORLD PR# 731
      *  BAH 110601 ADDED SP-SCFRMLA 52 AND 53 FOR MULLEN PR# 3799
      *  MJD 110615 ADDED SP-ROUNF-MFEE. THIS WILL FORCE CALCZL TO
      *             ROUND UP THE MAINTFEE FOR MEXICO MFFRMLA 05
      *             LOANS WHILE ALLOWING EXISTING CANS TO WORK AS BEFORE.
      *                                                               WORLD #692
      *  MJD 130530 ADDED SP-SCFRMLA = 54.  SIMILAR TO 12 BUT TAKES EXTRA 4%
      *                   AT THE END.                                WORLD #0874
      *  BAH 130814 ADDED SP-SCFRMLA = 55. WORLD PR# 918
      *  BAH 131213 ADDED SP-SCFRMLA = 56. WORLD TX PR# 953
      *  BAH 150508 ADDED SP-SCFRMLA = 57. NSA PR# 4747
      *  BAH 150508 ADDED SP-SCFRMLA = 58 TX WORLD PR# 1154
      *  CS 160728 CHANGED SAVEKEY FROM X(8) TO X(9)
      *  CS 170505 ACTIVATE 8 DIGIT DATES
      *  BAH 2019.0529 ADDED SP-SCFRMLA = 59 KY HB 225 WORLD PR# 1374
      *  BAH 2021.0615 ADDED SP-SCFRMLA = 60 & 61 TN HB 421/SB 344 #1500
      *  BAH 2023.0608 ADDED SP-SCFRMLA 62, TN, WORLD #1608
      *  BAH 2024.1008 ADDED SP-SCFRMLA 63, WI, WORLD #1682 S35Q-141
      **************************************************************************
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
           "@(#)MN/SPMAN3.CBL S34 06/06/21-16:54:46 >".
      *    **********************************
      *    *      PROGRAM WORKERS
      *    **********************************
       COPY "LIBLP/LP01SP.CPY".
       COPY "LIBLP/LP01SPA.CPY".
       COPY "LIBLP/LP01SPB.CPY".
       COPY "LIBLP/LP01SPC.CPY".
       COPY "LIBLP/LP01SP_SQL.CPY".
       COPY "LIBLP/LPWSSP.CPY".
       COPY "LIBLP/LP01ST.CPY".
       COPY "LIBLP/LP01ST_SQL.CPY".
       COPY "LIBLP/LPWSST.CPY".
       COPY "LIBLP/LP01LC.CPY".
       COPY "LIBLP/LP01LC_SQL.CPY".
       COPY "LIBLP/LPWSLC.CPY".
       01  ONE               PIC S9.
       01  S                 PIC 99     COMP-3.
       01  ELE               PIC 99     VALUE 0.
       01  ELE-OVER-20       PIC 99     VALUE 0.
       01  SUB               PIC 99  COMP-3  VALUE 0.
       COPY "LIBLP/SPMANSEL.CPY".
       01  IO-TYPE           PIC X(3)   VALUE SPACES.
       01  NEW-FLAG          PIC 9      VALUE 0.
       01  FIRST-SCAN        PIC 9      VALUE 0.

       01  ORIGINAL-SP-REC      PIC X(SP-SIZE) VALUE SPACES.
       01  CHANGED-SP-REC       PIC X(SP-SIZE) VALUE SPACES.

       01  SPMAIN-LINK    VALUE SPACES.
           03  FORM-NAME     PIC X(20).
           03  SAVEKEY       PIC X(9).
           03  NEW-REC       PIC X.
           03  ERRCD         PIC X.
           03  FORMSEL       PIC 99.
           03  SPMAIN-WINDOW-HANDLE PIC X(10).

      *    **********************************
      *    *      SCREEN DISPLAY BUFFER
      *    **********************************
       01  DISPLAY-BUF     VALUE SPACES.
           03  F01           PIC X.
           03  F02           PIC ZZZ.
           03  F03           PIC ZZ.
           03  F04           PIC ZZ.
           03  F05           PIC ZZZ.
           03  F06           PIC X.
           03  F07           PIC X.
           03  F08           PIC ZZZ.
           03  F09           PIC ZZZ.
           03  F10           PIC ZZ.
           03  F11           PIC ZZ.
           03  F12           PIC ZZZZZ9.99  BLANK ZERO.
           03  F13           PIC ZZ.
           03  F14           PIC Z9.99      BLANK ZERO.
           03  F15           PIC ZZZ9.99    BLANK ZERO.
           03  F16           PIC ZZZZZ9.99  BLANK ZERO.
           03  F17           PIC ZZ.
           03  F171          PIC X.
           03  F18           PIC Z9.99      BLANK ZERO.
           03  F19           PIC ZZZ9.99    BLANK ZERO.
           03  F20           PIC ZZZ9.99    BLANK ZERO.
           03  F21           PIC X.
           03  F22           PIC ZZZ.
           03  F23           PIC X.
           03  ACC-RATE      OCCURS 5.
               05  A1-RATE   PIC ZZ9.999  BLANK ZERO.
               05  A1-AMT    PIC ZZZZZ9.99  BLANK ZERO.

       01  DISPLAY-LISTS.
           03  FILLER        PIC X(36)  VALUE
           "010 020 030 040 050 060 070 080 090 ".
           03  FILLER        PIC X(48)  VALUE
           "100 110 120 130 140 150 160 170 171 180 190 200 ".
           03  FILLER        PIC X(36)  VALUE
           "210 220 230 240 241 250 251 260 261 ".
           03  FILLER        PIC X(16)  VALUE
           "270 271 280 281.".
       01  DISPLAY-LIST REDEFINES DISPLAY-LISTS
                             PIC X(136).

      *    **********************************
      *    *   ELEMENT SPECIFICATIONS
      *    **********************************
       01  MAX          PIC 99 COMP-3 VALUE 28.
       01  SPEC-TABLE.
      * 1. SP-INTMETHOD
           03  FILLER        PIC X(8)   VALUE "E  A010 ".
      * 2. SP-YEARTYPE
           03  FILLER        PIC X(8)   VALUE "ENNN030 ".
      * 3. SP-PYEXFRMLA
           03  FILLER        PIC X(8)   VALUE "ENNN020 ".
      * 4. SP-PYEXSPCODE
           03  FILLER        PIC X(8)   VALUE "ENNN020 ".
      * 5. SP-PYEXYEARTP
           03  FILLER        PIC X(8)   VALUE "ENNN030 ".
      * 6. SP-PYEXFMDATE
           03  FILLER        PIC X(8)   VALUE "E  A010 ".
      * 7. SP-PYEXBASECD
           03  FILLER        PIC X(8)   VALUE "E  A010 ".
      * 8. SP-PYEXBASE
           03  FILLER        PIC X(8)   VALUE "ENNN030 ".
      * 9. SP-PYEXDAYS
           03  FILLER        PIC X(8)   VALUE "ENNN030 ".
      * 10. SP-MAXAPR-FRMLA
           03  FILLER        PIC X(8)   VALUE "ENNN020 ".
      * 11. SP-MINSF-FRMLA
           03  FILLER        PIC X(8)   VALUE "ENNN020 ".
      * 12. SP-MINSF-CHG
           03  FILLER        PIC X(8)   VALUE "ENNN062 ".
      * 13. SP-SCFRMLA
           03  FILLER        PIC X(8)   VALUE "ENNN020 ".
      * 14. SP-SCRATE
           03  FILLER        PIC X(8)   VALUE "ENNN022 ".
      * 15. SP-SCMIN
           03  FILLER        PIC X(8)   VALUE "ENNN042 ".
           03  FILLER        PIC X(8)   VALUE "ENNN062 ".
           03  FILLER        PIC X(8)   VALUE "ENNN020 ".
           03  FILLER        PIC X(8)   VALUE "ENNN022 ".
           03  FILLER        PIC X(8)   VALUE "ENNN042 ".
      * 20. SP-MFMAX
           03  FILLER        PIC X(8)   VALUE "ENNN042 ".
      * 21. SP-ACC-FRMLA
           03  FILLER        PIC X(8)  VALUE "E  A010 ".
      * 22. SP-ACC-YRTYPE
           03  FILLER        PIC X(8)  VALUE "ENNN030 ".
      * 23. SP-ACC-INTMETH
           03  FILLER        PIC X(8)  VALUE "E  A010 ".
      * 24. SP-ACC-RATE(1)
           03  FILLER        PIC X(8)  VALUE "ENNN033 ".
           03  FILLER        PIC X(8)  VALUE "ENNN033 ".
           03  FILLER        PIC X(8)  VALUE "ENNN033 ".
           03  FILLER        PIC X(8)  VALUE "ENNN033 ".
      * 25. SP-ACC-RATE(5)
           03  FILLER        PIC X(8)  VALUE "ENNN033 ".
       01  SPEC-TAB       REDEFINES SPEC-TABLE.
           03  SPEC-REC   OCCURS 28 TIMES.
               05  SPEC     PIC X(7).
               05  CHFG     PIC X.

       COPY "LIBWI/SPSCANW.CPY".
       COPY "LIBCL/CLMENUW_EXT.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBWI/MEMOSPW.CPY".
       COPY "LIBWI/LCSCANW.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBMN/SPMAN3_DEF.CPY".
       COPY "LIBMN/SPMAN3_WKS.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/ACCESSW.CPY".

       COPY "LIBGB/GETFMW.CPY".
       01 FORM-LINK-AREA    PIC X(2000).

       SCREEN SECTION.
       COPY "LIBMN/SPMAN3_SCN.CPY".

       PROCEDURE DIVISION
            USING FORM-PATHNAME FORM-LINK-AREA EXIT-PATHNAME.

      ******************************************
      *      MAIN PROGRAM MODULE
      ******************************************
       MAIN-PROGRAM SECTION.
           PERFORM SQL-CONNECT.

       COPY "LIBGB/CHKSEC.CPY".
           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "SPMAN3" TO VDU-FORM-NAME.
           MOVE " " TO VDU-WIMENU-SELECTION
                       VDU-PGMENU-SELECTION.
             MOVE FORM-LINK-AREA TO SPMAIN-LINK.

      * THIS IS A TEST ONLY:
      * ALLOW CTRL 'N' INTERRUPT:
      *    MOVE "WI/YORCAL" TO VDU-CTRL-N-PROG.
      * THIS IS A TEST ONLY:

       MAIN-MODULE.
           PERFORM FORM-RESET.
           PERFORM REC-SP-MODULE.
           IF IO-TYPE = "END"
              MOVE "X" TO ERRCD
              GO TO END-ROUTINE.
           IF IO-TYPE = "CLR"
              GO TO MAIN-MODULE.
           PERFORM ENTRY-MODULE.
           IF IO-TYPE = "RWR"
              PERFORM WRITE-MODULE.
           IF NEW-REC = "Y"
              MOVE "P" TO FORM-P1
              MOVE 4 TO FORM-SEL1.
           IF FORM-SEL1 NOT = 0
              IF FORM-P1 = "P"
                 MOVE SP1-KEY TO SAVEKEY
                 MOVE FORM-SEL1 TO FORMSEL
                 GO TO END-ROUTINE.
           IF FORM-SEL2 NOT = 0
              IF FORM-P2 = "P"
                 MOVE SP1-KEY TO SAVEKEY
                 MOVE FORM-SEL2 TO FORMSEL
                 GO TO END-ROUTINE.
           GO TO MAIN-MODULE.

      ******************************************
       REC-SP-MODULE SECTION.
      ******************************************
       REC-SP-MODULE-NO.
           MOVE SPACES TO IO-TYPE.
           IF SAVEKEY NOT = " "
              MOVE SAVEKEY TO SP1-KEY
              MOVE " " TO SAVEKEY
              PERFORM DISPLAY-SCAN-KEY
              GO TO READ-SPFILE.
           MOVE 0 TO NEW-FLAG.
           MOVE "SPCLASS LCDESC CLASS STDESC SPNO SUBCLASS." TO WR-LIST.
           MOVE SPACES TO WR-BUF.
           PERFORM CALL-WRFORM.
           MOVE "E  A020SPNO." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>"
             MOVE "END" TO IO-TYPE
             GO TO END-REC-SP-MODULE.
           IF VDUSTATUS = "1<"
             PERFORM ENTER-SCANKEY
             GO TO END-REC-SP-MODULE.
           IF VDUSTATUS NOT = "00"
             GO TO REC-SP-MODULE-NO.
           MOVE VDUBUF TO SP-ORGST.
           PERFORM VERIFY-STATE-CODE.
           IF IO-FG NOT = 0
              GO TO REC-SP-MODULE-NO.
       REC-SP-CLASS.
           MOVE "LCDESC." TO WR-LIST.
           MOVE SPACES TO WR-BUF.
           PERFORM CALL-WRFORM.
           MOVE "EN N020CLASS." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10"
              GO TO REC-SP-MODULE-NO.
           IF VDUSTATUS = "1U"
              GO TO REC-SP-MODULE-NO.
           IF VDUSTATUS = "1:"
              PERFORM CLASS-SCAN
              IF IO-FG NOT = 0
                 GO TO REC-SP-CLASS
              ELSE
                 MOVE "SN N020CLASS." TO VDU
                 MOVE LCSCAN-KEY TO VDUNUM0
                 PERFORM SCREENIO
                 MOVE LCSCAN-KEY TO VDUNUM0.
           IF VDUSTATUS = "1<"
              MOVE SP-ORGST TO SPSCAN-ORGST
              MOVE 0 TO SPSCAN-SPRCLASS
              MOVE 0 TO SPSCAN-SUBCLASS
                        SPSCAN-LAWCODE
              PERFORM SPSCAN-SCAN
              IF IO-FG = 9
                 GO TO REC-SP-CLASS
              ELSE
                 MOVE "SNNN020CLASS." TO VDU
                 MOVE SPSCAN-SP-SPRCLASS TO VDUNUM0
                                          SP-SPRCLASS
                 PERFORM SCREENIO
                 MOVE "SNNN020SUBCLASS." TO VDU
                 MOVE SPSCAN-SP-SUBCLASS TO VDUNUM0
                                             SP-SUBCLASS
                 PERFORM SCREENIO
                 MOVE "SNNN020SPCLASS." TO VDU
                 MOVE SPSCAN-SP-LAWCODE TO VDUNUM0
                                           SP-LAWCODE
                 PERFORM SCREENIO
                 GO TO READ-SPFILE.

           IF VDUSTATUS NOT = "00"
             GO TO REC-SP-CLASS.
           MOVE VDUNUM0 TO SP-SPRCLASS.
           PERFORM VERIFY-LOAN-CLASS.
           IF IO-FG NOT = 0
              GO TO REC-SP-CLASS.
       REC-SP-SUBCLASS.
           MOVE "EN N020SUBCLASS." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10"
              GO TO REC-SP-MODULE-NO.
           IF VDUSTATUS = "1U"
              GO TO REC-SP-CLASS.
           IF VDUSTATUS = "1<"
              MOVE SP-ORGST TO SPSCAN-ORGST
              MOVE SP-SPRCLASS TO SPSCAN-SPRCLASS
              MOVE 0 TO SPSCAN-SUBCLASS
                        SPSCAN-LAWCODE
              PERFORM SPSCAN-SCAN
              IF IO-FG = 9
                 GO TO REC-SP-SUBCLASS
              ELSE
                 MOVE "SNNN020SUBCLASS." TO VDU
                 MOVE SPSCAN-SP-SUBCLASS TO VDUNUM0
                                            SP-SUBCLASS
                 PERFORM SCREENIO
                 MOVE "SNNN020SPCLASS." TO VDU
                 MOVE SPSCAN-SP-LAWCODE TO VDUNUM0
                                           SP-LAWCODE
                 PERFORM SCREENIO
                 GO TO READ-SPFILE.
           IF VDUSTATUS NOT = "00"
              GO TO REC-SP-SUBCLASS.
           MOVE VDUNUM0 TO SP-SUBCLASS.
       REC-SP-LAWCODE.
           MOVE "EN N020SPCLASS." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10"
              GO TO REC-SP-MODULE-NO.
           IF VDUSTATUS = "1U"
              GO TO REC-SP-CLASS.
           IF VDUSTATUS = "1<"
              MOVE SP-ORGST TO SPSCAN-ORGST
              MOVE SP-SPRCLASS TO SPSCAN-SPRCLASS
              ADD 1 SP-SUBCLASS GIVING SPSCAN-SUBCLASS
              MOVE 0 TO SPSCAN-LAWCODE
              PERFORM SPSCAN-SCAN
              IF IO-FG = 9
                 GO TO REC-SP-LAWCODE
              ELSE
                 MOVE "SNNN020SPCLASS." TO VDU
                 MOVE SPSCAN-SP-LAWCODE TO VDUNUM0
                                           SP-LAWCODE
                 PERFORM SCREENIO
                 GO TO READ-SPFILE.
           IF VDUSTATUS = "1U"
              GO TO REC-SP-SUBCLASS.
           IF VDUSTATUS NOT = "00"
              GO TO REC-SP-LAWCODE.
           MOVE VDUNUM0 TO SP-LAWCODE.
       READ-SPFILE.
           PERFORM OPEN-SP1-FILE.
           PERFORM READ-SP1-FILE.
           PERFORM CLOSE-SP1-FILE.
           IF IO-FG NOT = 0
              GO TO REC-SP-MODULE-NO.
           MOVE SP-REC TO ORIGINAL-SP-REC.
           PERFORM DISPLAY-FIELDS.
           MOVE 0 TO NEW-FLAG.
           IF NEW-REC = "Y"
              MOVE 1 TO NEW-FLAG.
       END-REC-SP-MODULE.
           EXIT.

      ******************************************
       CLASS-SCAN SECTION.
      ******************************************
           MOVE 0      TO LCSCAN-KEY.
           MOVE SPACES TO LCSCAN-VERIFY.
           MOVE "WI/LCSCAN" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH LCSCAN-BUF EXIT-PATHNAME.
           CANCEL FORM-PROGX.
           IF LCSCAN-STATUS NOT = "00" OR LCSCAN-KEY = 0
              MOVE 9 TO IO-FG
              GO TO EXIT-CLASS-SCAN.

           MOVE 0 TO IO-FG.

       EXIT-CLASS-SCAN.
           EXIT.

      ******************************************
       ENTER-SCANKEY SECTION.
      ******************************************
           MOVE 0 TO SP-SPRCLASS SP-SUBCLASS SP-LAWCODE FIRST-SCAN.
           MOVE " ENTER STATE CODE, OR F6 TO SCAN" TO MESS.
           PERFORM CALL-TMMSG.

           PERFORM OPEN-SP1-FILE.
           PERFORM START-SP1-FILE.
       ENTER-KEY.
           MOVE "E RA020SCANKEY." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO CLOSE-SCANKEY.
           IF VDUSTATUS = "1<"
              GO TO READ-FILES-NEXT.
           IF VDUSTATUS NOT = "00"
              GO TO ENTER-KEY.
           IF VDUBUF = SPACES
              IF FIRST-SCAN NOT = 0
                 MOVE SP-REC TO ORIGINAL-SP-REC
                 GO TO CLOSE-SCANKEY
              ELSE
                 GO TO ENTER-KEY.
           MOVE VDUBUF TO SP-ORGST.
           MOVE 0 TO SP-SPRCLASS SP-SUBCLASS SP-LAWCODE.
           PERFORM START-SP1-FILE.
       READ-FILES-NEXT.
           PERFORM READ-SP1-FILE-NEXT.
           IF IO-FG = 9
              MOVE "CLR" TO IO-TYPE
              GO TO CLOSE-SCANKEY.
      *    UNLOCK SPA-FILE.
      *    UNLOCK SPB-FILE.
      *    UNLOCK SPC-FILE.
           PERFORM DISPLAY-SCAN-KEY.
           PERFORM DISPLAY-FIELDS.
           MOVE 1 TO FIRST-SCAN.
           GO TO ENTER-KEY.
       DISPLAY-SCAN-KEY.
           MOVE "S  A020SPNO." TO VDU.
           MOVE SP-ORGST TO VDUBUF.
           PERFORM SCREENIO.
           PERFORM VERIFY-STATE-CODE.
           MOVE "SN N020CLASS." TO VDU.
           MOVE SP-SPRCLASS TO VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SN N020SUBCLASS." TO VDU.
           MOVE SP-SUBCLASS TO VDUNUM0.
           PERFORM SCREENIO.
           PERFORM VERIFY-LOAN-CLASS.
           PERFORM DISPLAY-SUBCLASS-DESC.
           MOVE "SN N020SPCLASS." TO VDU.
           MOVE SP-LAWCODE TO VDUNUM0.
           PERFORM SCREENIO.
       CLOSE-SCANKEY.
           PERFORM CLOSE-SP1-FILE.
           MOVE SPACES TO MESS.
           PERFORM CALL-TMMSG.
       END-SCANKEY.
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
              MOVE "N" TO NEW-REC
              GO TO VERIFY-ACCEL-TABLE.
           IF F2-KEY
              MOVE SP1-KEY      TO MEMOSP-SP1-KEY
              MOVE "WI/MEMOSP" TO FORM-NAM
              CALL FORM-PROGX USING FORM-PATH MEMOSP-BUF EXIT-PATHNAME
              CANCEL FORM-PROGX
              GO TO MODIFY-ROUTINE.
           IF VDUSTATUS NOT = "00"
              GO TO MODIFY-ROUTINE.
           MOVE VDUBUF TO ACTION.
           IF ACTION = SPACES
              MOVE "RWR" TO IO-TYPE
              GO TO VERIFY-ACCEL-TABLE.
      * FORM-NUM1 CHECKS FOR PAGES 1 THRU 9
           IF FORM-KEY1 = " " AND FORM-P1 = "P" AND FORM-SEL1 NUMERIC
              IF FORM-SEL1 < 1 OR FORM-SEL1 = 3
                 GO TO MODIFY-ROUTINE
              ELSE
                 GO TO CHECK-PAGE.
      * FORM-NUM2 CHECKS FOR PAGE 10
           IF FORM-KEY2 = " " AND FORM-P2 = "P" AND FORM-SEL2 NUMERIC
              IF NOT FORM-2-DIGITS
                 GO TO MODIFY-ROUTINE
              ELSE
                 GO TO CHECK-PAGE.
           GO TO CHK-CHNO.
       CHECK-PAGE.
           MOVE "RWR" TO IO-TYPE.
           MOVE 1 TO IO-FG.
           GO TO VERIFY-ACCEL-TABLE.
      *    GO TO EXIT-ENTRY-MODULE.
       CHK-CHNO.
           INSPECT ACTION REPLACING ALL SPACES
              BY ZEROS.
           IF CHNO-X NOT NUMERIC
              GO TO MODIFY-ROUTINE.
           IF CHNO = 0
              GO TO MODIFY-ROUTINE.
           IF CHNO > MAX
              GO TO MODIFY-ROUTINE.
           MOVE CHNO TO ELE.
       ENTER-ELE.
           IF CHKEY NOT = ZEROS
              IF CHFG(ELE) = " "
                 IF NEW-FLAG NOT = 1
                    GO TO MODIFY-ROUTINE.
           IF CHFG(ELE) = "N"
              IF NEW-FLAG = 1
                 GO TO CREATE-ROUTINE
              ELSE
                IF CHKEY NOT = EXT-CONAME-DAILY-PASSWORD
                   GO TO MODIFY-ROUTINE.
           MOVE SPEC(ELE) TO VDU.
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
       TEST-F1.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              MOVE "N" TO NEW-REC
              GO TO END-ENTRY-MODULE.
           IF VDUSTATUS NOT = "00"
              GO TO ENTER-ELE.
           MOVE 1 TO ONE.
       GOTO-FIELD.
           IF ELE > 20
              SUBTRACT 20 FROM ELE GIVING ELE-OVER-20
              GO TO OVER-20.
           GO TO A-01 A-02 A-03 A-04 A-05 A-06 A-07 A-08 A-09 A-10
                 A-11 A-12 A-13 A-14 A-15 A-16 A-17 A-18 A-19 A-20
              DEPENDING ON ELE.
           GO TO END-ENTRY-MODULE.
       OVER-20.
           GO TO A-21 A-22 A-23 A-24 A-25 A-26 A-27 A-28
             DEPENDING ON ELE-OVER-20.
           GO TO VERIFY-ACCEL-TABLE.

      *    **********************************
      *    *      SET RECORD ELEMENTS
      *    **********************************
       A-01.
           MOVE VDUBUF TO SP-INTMETHOD.
           IF NOT SP-INTMETHOD-VALID
              MOVE "ENTER: 'O' OR 'E'" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       A-02.
           MOVE VDUNUM0 TO SP-YEARTYPE.
           IF NOT SP-YEARTYPE-VALID
              MOVE "ENTER: 358, 360, 365, 367, OR 999" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       A-03.
           MOVE VDUNUM0 TO SP-PYEXFRMLA.
           IF NOT SP-PYEXFRMLA-VALID
              MOVE "ENTER: 0 OR 1 THRU 8 OR 10 THRU 12" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       A-04.
           MOVE VDUNUM0 TO SP-PYEXSPCODE.
           IF NOT SP-PYEXSPCODE-VALID
              MOVE "ENTER: 0 OR 1, ONLY" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       A-05.
           MOVE VDUNUM0 TO SP-PYEXYEARTP.
           IF NOT SP-PYEXYEARTP-VALID
              MOVE "ENTER: 358, 360, 365, 367 OR 999" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       A-06.
           MOVE VDUBUF TO SP-PYEXFMDATE.
           IF NOT SP-PYEXFMDATE-VALID
              MOVE "ENTER: 'L' OR 'I'" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           PERFORM SEND-PYEXFMDATE-DESC.
           GO TO CREATE-ROUTINE.
       A-07.
           MOVE VDUBUF TO SP-PYEXBASECD.
           IF NOT SP-PYEXBASECD-VALID
              MOVE "ENTER: 'D' OR 'M'" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           PERFORM SEND-PYEXBASECD-DESC.
           GO TO CREATE-ROUTINE.
       A-08.
           MOVE VDUNUM0 TO SP-PYEXBASE.
           GO TO CREATE-ROUTINE.
       A-09.
           MOVE VDUNUM0 TO SP-PYEXDAYS.
           IF SP-PYEXDAYS > 30
              MOVE "ENTER: < 30 DAYS ONLY" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       A-10.
           MOVE VDUNUM0 TO SP-MAXAPR-FRMLA.
           IF NOT SP-MAXAPR-FRMLA-VALID
              MOVE "ENTER: 0 ONLY" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       A-11.
           MOVE VDUNUM0 TO SP-MINSF-FRMLA.
           IF NOT SP-MINSF-FRMLA-VALID
              MOVE "ENTER: 0 ONLY" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       A-12.
           MOVE VDUNUM2 TO SP-MINSF-CHG.
           GO TO CREATE-ROUTINE.
       A-13.
           MOVE VDUNUM0 TO SP-SCFRMLA.
           IF NOT SP-SCFRMLA-VALID
              MOVE "ENTER: 0 OR 1 THRU 63" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           IF SP-SCFRMLA = 2 OR 3 OR 11 OR 12 OR 13 OR 19
                             OR 35 OR 38 OR 54
              IF LC-LOANTYPE NOT = "P"
                 MOVE "PRE COMPUTE ACCOUNTS ONLY" TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-ELE.
           IF NEW-REC NOT = "Y"
              IF SP-SCFRMLA NOT = 0
                 IF SP-ERNFRMLA(3) = 0
                    MOVE "MUST HAVE SERCHG EARN FORMULA, PAGE 15"
                                                             TO MESS
                    PERFORM SEND-MESS
                    GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       A-14.
           MOVE VDUNUM2 TO SP-SCRATE.
           GO TO CREATE-ROUTINE.
       A-15.
           MOVE VDUNUM2 TO SP-SCMIN.
           GO TO CREATE-ROUTINE.
       A-16.
           MOVE VDUNUM2 TO SP-SCMAX.
           GO TO CREATE-ROUTINE.
       A-17.
           MOVE VDUNUM0 TO SP-MFFRMLA.
           IF NOT SP-MFFRMLA-VALID
              MOVE "ENTER: 0, 1, 2, 3, 4 OR 5" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           IF NEW-REC NOT = "Y"
              IF SP-MFFRMLA NOT = 0
                 IF SP-ERNFRMLA(4) = 0
                 MOVE "MUST HAVE MAINTFEE EARN FORMULA, PAGE 16"
                                                          TO MESS
                    PERFORM SEND-MESS
                    GO TO ENTER-ELE.
       A-171.
           IF NOT SP-MFFRMLA-05
              MOVE SPACE TO SP-ROUND-MFEE
              MOVE "S  A011171P." TO VDU
              MOVE SPACES TO VDUBUF
              PERFORM SCREENIO
              MOVE "S  A001171." TO VDU
              MOVE SPACES TO VDUBUF
                             SP-ROUND-MFEE
              PERFORM SCREENIO
              GO TO CREATE-ROUTINE.

           MOVE "S  A011171P." TO VDU.
           MOVE "ROUND(YN)? " TO VDUBUF.
           PERFORM SCREENIO.
           MOVE "E  A001171." TO VDU.
           PERFORM SCREENIO.
           MOVE VDUBUF TO SP-ROUND-MFEE.
           IF NOT SP-ROUND-MFEE-VALID
              MOVE "ENTER: 'Y' OR 'N' " TO MESS
              PERFORM SEND-MESS
              GO TO A-171.
           GO TO CREATE-ROUTINE.
       A-18.
           MOVE VDUNUM2 TO SP-MFRATE.
           GO TO CREATE-ROUTINE.
       A-19.
           MOVE VDUNUM2 TO SP-MFMIN.
           GO TO CREATE-ROUTINE.
       A-20.
           MOVE VDUNUM2 TO SP-MFMAX.
           GO TO CREATE-ROUTINE.
       A-21.
           MOVE VDUBUF TO SP-ACC-FRMLA.
           IF NOT SP-ACC-FRMLA-VALID
              MOVE "ENTER: SPACE, 'A' OR 'B' " TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       A-22.
           MOVE VDUNUM0 TO SP-ACC-YRTYPE.
           IF NOT SP-ACC-YRTYPE-VALID
              MOVE "ENTER: 358, 360, 365, 367 OR 999" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       A-23.
           MOVE VDUBUF TO SP-ACC-INTMETH.
           IF NOT SP-ACC-INTMETH-VALID
              MOVE "ENTER: 'E' OR 'O' " TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       A-24.
           PERFORM ENTER-RATE-TABLE.
           IF IO-FG = 9
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       A-25.
           PERFORM ENTER-RATE-TABLE.
           IF IO-FG = 9
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       A-26.
           PERFORM ENTER-RATE-TABLE.
           IF IO-FG = 9
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       A-27.
           PERFORM ENTER-RATE-TABLE.
           IF IO-FG = 9
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       A-28.
           PERFORM ENTER-RATE-TABLE.
           IF IO-FG = 9
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.

       VERIFY-ACCEL-TABLE.

           MOVE 5 TO S.
       VERIFY-LOOP.
           IF S = 0
              NEXT SENTENCE
           ELSE
           IF SP-ACC-RATE(S) = 0
              SUBTRACT 1 FROM S
              GO TO VERIFY-LOOP
           ELSE
           IF SP-ACC-BREAK(S) NOT = 0
              MOVE "BREAK FOR LAST RATE MUST BE 0.00" TO MESS
              PERFORM SEND-MESS
              COMPUTE ELE = S + 23
      *       MOVE ZEROS TO CHKEY
      *       GO TO ENTER-ELE.
              MOVE SPEC(ELE) TO VDU
              MOVE ELE TO VDUROW
              MOVE 0 TO VDUCOL
              MOVE "." TO VDUDECIMAL
              PERFORM SCREENIO
              PERFORM ENTER-RATE-TABLE
              GO TO VERIFY-ACCEL-TABLE.
           IF IO-FG = 1 GO TO EXIT-ENTRY-MODULE.
       END-ENTRY-MODULE.
           MOVE 0 TO FORM-SEL1 FORM-SEL2.
       EXIT-ENTRY-MODULE.
           EXIT.

      *********************************************
       ENTER-RATE-TABLE SECTION.
      *********************************************
           MOVE 0 TO IO-FG.
           SUBTRACT 23 FROM ELE GIVING S.
           MOVE VDUNUM3 TO SP-ACC-RATE(S).
           IF ( S = 1             ) AND ( VDUNUM3 = 999.998 )
              GO TO ENTER-AMOUNT.
           IF SP-ACC-RATE(S) > 999.995
              MOVE "RATE MUST BE LESS THAN 999.996" TO MESS
              PERFORM SEND-MESS
              MOVE 9 TO IO-FG
              GO TO ENTER-RATE-TABLE-EXIT.
           IF VDUNUM3 = 0
              PERFORM CLEAR-RATE-TABLE
              GO TO ENTER-RATE-TABLE-EXIT.
       ENTER-AMOUNT.
           MOVE "ENNN062  1." TO VDU.
           MOVE ELE TO VDUROW.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              SUBTRACT 1 FROM ELE
              GO TO ENTER-RATE-TABLE-EXIT.
           IF VDUSTATUS = "1D"
              MOVE SP-ACC-BREAK(S) TO VDUNUM2
              MOVE "00" TO VDUSTATUS.
           IF VDUSTATUS NOT = "00"
              GO TO ENTER-AMOUNT.
           MOVE VDUNUM2 TO SP-ACC-BREAK(S).
       ENTER-RATE-TABLE-EXIT.
           EXIT.

      ***************************************
       CLEAR-RATE-TABLE SECTION.
      ***************************************
       CLEAR-RATES.
           MOVE "S  A000  0." TO VDU.
           MOVE ELE TO VDUROW.
           PERFORM SCREENIO.
           MOVE 1 TO VDUCOL.
           PERFORM SCREENIO.

           MOVE 0 TO SP-ACC-RATE(S).
           MOVE 0 TO SP-ACC-BREAK(S).
           IF S < 5
              ADD 1 TO S ELE
              GO TO CLEAR-RATES.

       CLEAR-RATE-TABLE-EXIT.
           EXIT.


      **************************************
       WRITE-MODULE SECTION.
      **************************************
           PERFORM OPEN-SP1-FILE.
           MOVE SP-REC TO CHANGED-SP-REC.
           IF ORIGINAL-SP-REC = CHANGED-SP-REC
              GO TO END-WRITE-MODULE.
           PERFORM READ-SP1-FILE.
           IF IO-FG NOT = 0
              PERFORM UNDERGONE-CHANGE
              PERFORM RECORD-DELETED
              GO TO END-WRITE-MODULE.
           IF SP-REC NOT = ORIGINAL-SP-REC
              PERFORM UNDERGONE-CHANGE
              PERFORM CHANGE-AGAIN
              GO TO END-WRITE-MODULE.
           MOVE CHANGED-SP-REC TO SP-REC.
           PERFORM REWRITE-SP1-FILE.
           IF IO-GOOD
              MOVE FORM-PATHNAME TO FORM-PATH
              MOVE "CL/SPMEMO"   TO FORM-NAM
              CALL FORM-PROGX USING FORM-PATH
                                   ORIGINAL-SP-REC
                                   SP-REC
              CANCEL FORM-PROGX.

      * CODE TO WRITE CHANGE FILE.
           MOVE "C" TO ERRLOGW-ACTION.
           MOVE SP1-KEY TO ERRLOGW-KEY.
           MOVE FORM-PATHNAME TO ERRLOGW-PROGX.

           MOVE "CL/ERRLOG" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH ERRLOGW-AREA.
           CANCEL FORM-PROGX.

           GO TO END-WRITE-MODULE.

       END-WRITE-MODULE.
           PERFORM CLOSE-SP1-FILE.

      *************************************************
       SPSCAN-SCAN SECTION.
      *************************************************
           MOVE SPACES TO SPSCAN-SP1-KEY.
           MOVE SPACES TO SPSCAN-LAST-LAW.
           MOVE "WI/SPSCAN" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH SPSCAN-BUF EXIT-PATHNAME.
           CANCEL FORM-PROGX.
           IF SPSCAN-STATUS NOT = "00" OR SPSCAN-SP1-KEY = SPACES
              MOVE 9 TO IO-FG
              GO TO EXIT-SPSCAN-SCAN
           ELSE
              MOVE 0 TO IO-FG.

       EXIT-SPSCAN-SCAN.
           EXIT.

      *************************************************
       DISPLAY-FIELDS SECTION.
      *************************************************
           MOVE SPACES TO DISPLAY-BUF.
           MOVE "S  8000EFFDATE." TO VDU.
           MOVE SP-EFFDATE TO VDU-DATE-YYYYMMDD.
           PERFORM SCREENIO.
           MOVE "S  8000EXPDATE." TO VDU.
           MOVE SP-EXPDATE TO VDU-DATE-YYYYMMDD.
           PERFORM SCREENIO.
           PERFORM DISPLAY-SUBCLASS-DESC.

           MOVE SP-INTMETHOD TO F01.
           MOVE SP-YEARTYPE TO F02.
           MOVE SP-PYEXFRMLA TO F03.
           MOVE SP-PYEXSPCODE TO F04.
           MOVE SP-PYEXYEARTP TO F05.
           MOVE SP-PYEXFMDATE TO F06.
           MOVE SP-PYEXBASECD TO F07.
           MOVE SP-PYEXBASE TO F08.
           MOVE SP-PYEXDAYS TO F09.
           MOVE SP-MAXAPR-FRMLA TO F10.
           MOVE SP-MINSF-FRMLA TO F11.
           MOVE SP-MINSF-CHG TO F12.
           MOVE SP-SCFRMLA TO F13.
           MOVE SP-SCRATE TO F14.
           MOVE SP-SCMIN TO F15.
           MOVE SP-SCMAX TO F16.
           MOVE SP-MFFRMLA TO F17.

           IF SP-MFFRMLA-05
              MOVE "S  A011171P." TO VDU
              MOVE "ROUND(YN)? " TO VDUBUF
              PERFORM SCREENIO
              MOVE SP-ROUND-MFEE TO F171
           ELSE
              MOVE "S  A011171P." TO VDU
              MOVE SPACES TO VDUBUF
              PERFORM SCREENIO
              MOVE SPACE TO F171.

           MOVE SP-MFRATE TO F18.
           MOVE SP-MFMIN TO F19.
           MOVE SP-MFMAX TO F20.
           MOVE SP-ACC-FRMLA TO F21.
           MOVE SP-ACC-YRTYPE TO F22.
           MOVE SP-ACC-INTMETH TO F23.
           MOVE 0 TO SUB.
       NEXT-RATE.
           ADD 1 TO SUB.
           MOVE SP-ACC-RATE(SUB) TO A1-RATE(SUB).
           MOVE SP-ACC-BREAK(SUB) TO A1-AMT(SUB).
           IF SUB < 5
              GO TO NEXT-RATE.
           MOVE 0 TO SUB.
           PERFORM SEND-PYEXFMDATE-DESC.
           PERFORM SEND-PYEXBASECD-DESC.

           MOVE DISPLAY-BUF TO WR-BUF.
           MOVE DISPLAY-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
       DISPLAY-FIELDS-EXIT.
           EXIT.

      ****************************************************
       FORM-RESET SECTION.
      ****************************************************
           MOVE EXT-CONAME-CONAME TO SPMAN3-CONAME.
           MOVE EXT-CONAME-SYSDATE TO SPMAN3-SYSDATE.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY SPMAN3-SCREEN.
           DISPLAY SPMAN3-SCREEN UPON SPMAIN-WINDOW-HANDLE.
           MOVE SPMAN3-FORM-FIELDS TO WR-BUF.
           MOVE SPMAN3-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      ****************************************************
       SETUP-LINK-INFO SECTION.
      ****************************************************
           MOVE SPMAN3-HELP--FILE TO HELP-LINK-FILE.
           MOVE SPMAN3-HELP--DIR TO HELP-LINK-DIR.

      ****************************************************
       VDU-CONVERT-FIELD SECTION.
      ****************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBMN/SPMAN3_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT

           END-EVALUATE.

       COPY "LIBLP/LPTBNO.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/DATER.CPY".

      ****************************************************
       MISC-PARAGRAPHS SECTION.
      ****************************************************
       COPY "LIBGB/ACCESS.CPY".

       VERIFY-STATE-CODE.
           MOVE SP-ORGST TO ST-CODE.
           PERFORM OPEN-ST1-FILE.
           PERFORM READ-ST1-FILE.
           PERFORM CLOSE-ST1-FILE.
           IF IO-FG NOT = 0
              MOVE "* NOT ON FILE *" TO ST-DESC.
           MOVE "S  A160STDESC." TO VDU.
           MOVE ST-DESC TO VDUBUF.
           PERFORM SCREENIO.
       VERIFY-LOAN-CLASS.
           MOVE SP-SPRCLASS TO LC-CODE.
           PERFORM OPEN-LC1-FILE.
           PERFORM READ-LC1-FILE.
           PERFORM CLOSE-LC1-FILE.
           IF IO-FG NOT = 0
              MOVE "* NOT ON FILE *" TO LC-DESC.
           MOVE "S  A160LCDESC. " TO VDU.
           MOVE LC-DESC TO VDUBUF.
           PERFORM SCREENIO.
       DISPLAY-SUBCLASS-DESC.
           MOVE "S  A150DESC. " TO VDU.
           MOVE SP-DESC TO VDUBUF.
           PERFORM SCREENIO.
       SEND-PYEXFMDATE-DESC.
           MOVE "S  A000061." TO VDU.
           MOVE "INVALID" TO VDUBUF.
           IF SP-PYEXFMDATE = "L"
              MOVE "LOAN DATE" TO VDUBUF
           ELSE
              IF SP-PYEXFMDATE = "I"
                 MOVE "INTRST DTE" TO VDUBUF.
           PERFORM SCREENIO.
       SEND-PYEXBASECD-DESC.
           MOVE "S  A000071." TO VDU.
           MOVE "INVALID" TO VDUBUF.
           IF SP-PYEXBASECD = "D"
              MOVE "DAYS" TO VDUBUF
           ELSE
              IF SP-PYEXBASECD = "M"
                 MOVE "MONTHS" TO  VDUBUF.
           PERFORM SCREENIO.
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

      *************************************************
       IO-ROUTINES SECTION.
      *************************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-ST1-FILE.
           PERFORM CLOSE-SP1-FILE.
           PERFORM CLOSE-LC1-FILE.
       COPY "LIBLP/LPSTGS_SQL.CPY".
       COPY "LIBLP/LPSPGS_SQL.CPY".
       COPY "LIBLP/LPLCGS_SQL.CPY".
       COPY "LIBLP/LPSP1IN.CPY".
       COPY "LIBLP/LPST1RN.CPY".
       COPY "LIBLP/LPLC1RN.CPY".
       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO ERRCD.

      *************************************************
       END-ROUTINE SECTION.
      *************************************************
           PERFORM CLOSE-FILES.
       EXIT-PROG.
            MOVE FORM-PATH TO FORM-PATHNAME.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY SPMAN3-SCREEN.
           MOVE SPMAIN-LINK TO FORM-LINK-AREA.
       STOP-RUN.
           EXIT PROGRAM.
