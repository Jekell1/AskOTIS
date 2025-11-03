       IDENTIFICATION DIVISION.
       PROGRAM-ID.      SPMAN1.
      ***************************************************************
      *   DIR :  MN
      *   DESC:  STATE PARAMETER MAINT - PROG 1
      *
      * HOW TO DELETE A SPR
      *      CALL UP THE SPR
      *      TYPE 3535 & RETURN
      *      TYPE DELETE & RETURN
      *      TYPE 3535 & RETURN
      *    IF YOU ARE IN A BRANCH WITH LOANS, IT WILL STILL CHECK IF
      *    LOANS EXIST.
      *    THIS CHANGE WAS TO PREVENT A HOME OFFICE BRANCH WITH NO
      *    LOANS TO NOT BE ALLOWED TO DELETE A SPR
      *
      * REV:
      *  MJD 050895 ADDED CODE TO CREATE CHANGE FILE AFTER REWRITE
      *  JTG 062395 CHANGED TO GO TO PAGE 19
      *  JTG 070695 ADDED SP-LCFRMLA = "L", MERCURY 'UCCC'
      *  JTG 082595 ADDED SP-LCFRMLA = "M", MERCURY 'LA'
      *  BAH 111595 ADDED SP-LCTYPE = "N", MERCURY 'TN'
      *  BAH 112995 ADDED SP-LCFRMLA = "N", MERCURY 'NV'
      *  JFF 070896 CHANGED TO GO TO PAGE 20, MOVED EXPIRED LOAN FIELDS
      *             TO PAGE 20, ADDED SP-BANKUNITPER-CD, SP-TRWBANKRULE,              *             SP-TRWCONTPDTH, SP-TRWBANKUNITPER-CD, SP-LCUNITPER-CD
      *  GLF 091296 ADDED FIELD 30: SUBLASS DESC.
      *  GLF 111596 ADDED SP-RESCIND-BUS-DAYS.
      *  KEC 031297 ADDED SP-RESCIND-SATURDAY; PR#459.
      *  JTG 082897 NEW SP-BANKUNITPER-CD & SP-TRWBANKUNITPER-CD 'D'-INSTANTAUTO
      *  JTG 091797 ADDED 'D' WORLD A90, SP-BANKRULE & SP-TRWBANKRULE
      *  BLV 061198 CHANGE ORIG, CHANGED RECS TO USE SP-SIZE(4513) NOT 4502
      *  KEC 071398 ADDED SP-CAN-LOANS-FG (#15)                  PR# 187
      *  JTG 091697 ADDED 'R' REGENCY REVOL, TO SP-BANKRULE & SP-TRWBANKRULE
      *             ADDED 'R' REGENCY REVOL, TO SP-LCFRMLA
      *  JTG 122198 ADDED SP-LCTYPE = 'P'  OHIO PRIOR 07/12/96, REGENCY #002
      *  JTG 990121 ADDED 88 SP-LCTYPE-REVOL-VALID REGENCY, REVOL
      *  MJD 990203 ADDED RECPAYCD AND RECPDTH AND RECFRMLA "M"
      *  KEC 990211 CHANGED SP-RECIND-BUS-DAYS-VALID FROM MAX OF 5 DAYS
      *              TO A MAX OF 8 DAYS.
      *  GLF 030299 ADDED SP-LCTYPE-VALID OF "Q" FOR MULLEN, CALCULATE FLAT
      *             CHARGE BASED ON DAYS LATE PAST GRACE PERIOD
      *             10.00 IF < 5, OR 15.00 > OR = 5
      *  JTG 990311 ADDED SP-BANKUNITPER-CD & SP-TRWBANKUNITPER-CD = 'E' TFC
      *  JTG 990419 ADDED SP-RECFRMLA-CALENDAR-DUEDAY  "D"         FROM REL: A50
      *                   SP-RECFRMLA-PERCNT-PMT       "P"         MERCURY  #183
      *  JTG 990624 ADDED SP-BANKRULE & SP-TRWBANKRULE = 'S' REVOL REGWAN   #044
      *  MJD 990721 ADDED TEST TO PREVENT CHANGING OF EXPDATE IF A NEW SPR
      *             EXISTS.
      *  JTG 990908 CHANGED SP-BANKRULE & SP-TRWBANKRULE = 'R' & 'S' DESCRIPTION
      *             REVOL REGWAN   #044
      *  BAH 000322 ADDED "010101" FROM SEL ENTRY TO CHANGE EXPDATE FROM MJD'S
      *             CHANGE IN JULY 99, LORI NEEDED TO CHG REGWAN'S EXPDATES
      *  JTG 000724 ADDED LOGIC FOR LCFRMLA = "H"  RE: TOTAL MONIES REGMGM #1262
      *  MJD 000928 ADDED F2-MEMO OPTION FOR NEW SPR MEMO FILE (SMFILE)
      *  JTG 010417 CHANGED NEW RATE TYPE 'Z' AMORTIZED TO NOT BE ALLOWED WHEN
      *             TOTAL MONEY LATE CHARGE FORMULA EXISTS
      *                                               LENDMARK PR#1045 & PR#1380
      *  JTG 010625 CREATED LCAP-X (BAD LOGIC) BY PUTTING BACK THE OLD LOGIC
      *             FOR LCAP-E                                     IDEAL PR#1509
      *  JTG 030528 ADDED SP-LCFRMLA = 'P' LIKE 'I'                  PACE #408
      *  BAH 040116 LOGGED DELETE OF AN SPR
      *  BAH 040331 ADDED SP-LC-ON-JUDGEMENTS FOR RIVERSIDE, PR# 2272, LAW SAYS
      *             CANT CHARGE LATE CHARGE ON JUDGEMENT ACCOUNTS
      *  BAH 040909 PASSWORD PAGE 1 OF SPR'S, REGACC PR# 79
      *  CS  040927 ADDED CALL TO CL/SPMEMO WHICH WILL WRITE A SPR CHANGE MEMO
      *  CS  050325 ADD SP-INSURANCE-WRITTEN.. INDICATES IF YOU CAN WRITE ANY
      *             TYPE OF INSURANCE IN THIS SPR [LENDMARK 2479]
      *  CS  070227 ADDED SP-AUTO-ASSESS-LATE-CHG PR#152-R REGIONAL,
      * MJD 090424 ADDED SP-BANKRULE-RATE                 (BARNETT)  71284 #3189
      *  CS 100624 ADDED SP-UNITPER-CD FOR SEMI-MONTHLY MEXICO WORLD PR#692
      * BAH 110117 USED BLANK FIELD 27 FOR SP-RENEWAL-EDIT FOR WORLD PR# 734
      *  CS 130214 A30 CONVERSION STARTED
      * BAH 130314 ADDED 3535 LOGIC TO DELETE SPR, NOTE AT TOP, REGMGM PR# 4223
      * MJD 130822 ALLOW LCFRMLA "S" WITH FOR USE WITH IB.           71269 #4326
      * BAH 130916 ADDED SP-RENEWAL-EDIT "B" AND "C" FOR WORLD, PR# 923
      * MJD 131113 CLEANED UP A FEW ERRORS AND FINISHED A30 CONVERSION.
      * BAH 151103 ADDED SP-RENEWAL-EDIT "D" AND "E"  FOR REGMGM PR# 4885
      *  CS 160728 REPLACED LN2-KEY WITH LN3-KEY, SAVEKEY FROM X(8) TO X(9)
      *  CS 160805 UNCOMMENTED MOVE OF SP1-KEY TO MEMOSP-SP1-KEY, SP-SPRCLASS
      *            IS NOW 9(3)
      *  BAH 170424 ACTIVATE 8 DIGIT DATES, PD0006
      *   CS 170505 CHANGED A-01  IF SP-EXPDATE NOT = 0 AND VDUNUM0 NOT =
      *             SP-EXPDATE TO NOT = VDU-DATE-YYYYMMDD
      * BAH 181212 GLOBAL LNFILE
      * BAH 20190625 REMOVED LC-REVOLVING, PT#0153
      * BLM 2021 WASN'T ALLOWING 'D' FOR SP-TRWBANKRULE, IT WAS IN COLUMN
      *          74 OF THE 88 LEVEL
      * BAH 2023.0328 ADDED SP-LCFRMLA "T" FOR "WI" #1597
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
           "@(#)MN/SPMAN1.CBL S35 02/21/24-12:34:57 >".
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
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
      *    **********************************
      *    *      PROGRAM WORKERS
      *    **********************************

       01  ONE               PIC S9.
       01  SUB               PIC 99     COMP-3 VALUE 0.
       01  ELE               PIC 99     VALUE 0.
       COPY "LIBLP/SPMANSEL.CPY".

       01  IO-TYPE           PIC X(3)   VALUE " ".
       01  NEW-FLAG          PIC 9      VALUE 0.
       01  FIRST-SCAN        PIC 9      VALUE 0.
       01  EXPDATE-CHANGE-FG PIC X      VALUE "N".

       01  HOLD-EFFDATE      PIC 9(8)   COMP-3 VALUE 0.
       01  HOLD-EXPDATE      PIC 9(8)   COMP-3 VALUE 0.
       01  TODAYS-DATE       PIC 9(8)   COMP-3 VALUE 0.


       01  TEST-SP1-KEY                PIC X(09)     VALUE SPACES.
       01  FILLER            REDEFINES TEST-SP1-KEY.
           03  TEST-SP-ORGST           PIC X(02).
           03  TEST-SP-SPRCLASS        PIC 9(03).
           03  TEST-SP-SUBCLASS        PIC 9(02).
           03  TEST-SP-LAWCODE         PIC 9(02).

       01  ORIGINAL-SP-REC      PIC X(SP-SIZE) VALUE " ".
       01  CHANGED-SP-REC       PIC X(SP-SIZE) VALUE " ".

       01  SPMAIN-LINK    VALUE SPACES.
           03  FORM-NAME     PIC X(20).
           03  SAVEKEY       PIC X(9).
           03  NEW-REC       PIC X.
           03  ERRCD         PIC X.
           03  FORMSEL       PIC 99.
           03  SPMAIN-WINDOW-HANDLE PIC X(10).

      *    **********************************
      *    *      SCREEN DISPLAY BUFFER1
      *    **********************************
       01  DISPLAY-BUF      VALUE " ".
           03  NOTES         PIC X(60).
           03  A01           PIC 99/99/99 BLANK ZERO.
           03  A02           PIC X.
           03  A021          PIC ZZ9.999  BLANK ZERO.
           03  A022          PIC ZZZ.
           03  A023          PIC X.
           03  A03           PIC X.
           03  A032          PIC X.
           03  A04           PIC X.
           03  A041          PIC X.
           03  A05           PIC X.
           03  A06           PIC X.
           03  A07           PIC ZZZ.
           03  A071          PIC ZZZ.
           03  A08           PIC X.
           03  A09           PIC X.
           03  A10           PIC ZZZ.
           03  A11           PIC X.
           03  A12           PIC X.
           03  A121          PIC X.
           03  A122          PIC Z(3).
           03  A13           PIC X.
           03  A14           PIC X.
           03  A15           PIC X.
           03  A16           PIC X.
           03  A17           PIC X.
           03  A18           PIC ZZZ9.9999  BLANK ZERO.
           03  A19           PIC Z9.99      BLANK ZERO.
           03  A20           PIC ZZZ9.99    BLANK ZERO.
           03  A21           PIC Z9.
           03  A211          PIC X.
           03  A22           PIC ZZ9.
           03  A23           PIC ZZZ.
           03  A24           PIC X.
           03  A25           PIC X.
           03  A26           PIC X.
           03  A27           PIC X.
           03  A28           PIC X.
           03  A29           PIC X.
           03  A30           PIC X(15).
       01  DISPLAY-LIST.
           03  FILLER        PIC X(58)  VALUE
           "NOTES 010 020 021 022 023 030 032 040 041 050 060 070 071 ".
           03  FILLER        PIC X(44)  VALUE
           "080 090 100 110 120 121 122 130 140 150 160 ".
           03  FILLER        PIC X(56)  VALUE
           "170 180 190 200 210 211 220 230 240 250 260 270 280 290 ".
           03  FILLER        PIC X(4) VALUE
           "300.".


      *    **********************************
      *    *   ELEMENT SPECIFICATIONS
      *    **********************************
       01  MAX               PIC 99  VALUE 30.
       01  SPEC-TABLE.
      * 1. SP-EXPDATE
           03  FILLER        PIC X(8)   VALUE "ERNM060 ".
      * 2. SP-JUDGFRMLA
           03  FILLER        PIC X(8)   VALUE "E  A010 ".
      * 3. SP-1098FRMLA
           03  FILLER        PIC X(8)   VALUE "E  A010 ".
      * 4. SP-DISFRMLA
           03  FILLER        PIC X(8)   VALUE "E  A010 ".
      * 5. SP-CONTRFRMLA
           03  FILLER        PIC X(8)   VALUE "E  A010 ".
      * 6. SP-BANKRULE
           03  FILLER        PIC X(8)   VALUE "E  A010 ".
      * 7. SP-CONTPDTH
           03  FILLER        PIC X(8)   VALUE "ENNN030 ".
      * 8. SP-BANKUNITPER-CD
           03  FILLER        PIC X(8)   VALUE "E  A010 ".
      * 9. SP-TRWBANKRULE
           03  FILLER        PIC X(8)   VALUE "E  A010 ".
      * 10. SP-TRWCONTPDTH
           03  FILLER        PIC X(8)   VALUE "ENNN030 ".
      * 11. SP-TRWBANKUNITPER-CD
           03  FILLER        PIC X(8)   VALUE "E  A010 ".
      * 12. SP-RECFRMLA
           03  FILLER        PIC X(8)   VALUE "E  A010 ".
      * 13. SP-RESCIND-BUS-DAYS
           03  FILLER        PIC X(8)   VALUE "E  A010 ".
      * 14. SP-RESCIND-SATURDAY
           03  FILLER        PIC X(8)   VALUE "E  A010 ".
      * 15. SP-CAN-LOANS-FG
           03  FILLER        PIC X(8)   VALUE "E  A010 ".
      * 16. SP-LCFRMLA
           03  FILLER        PIC X(8)   VALUE "E  A010 ".
      * 17. SP-LCTYPE
           03  FILLER        PIC X(8)   VALUE "E  A010 ".
      * 18. SP-LCRATE
           03  FILLER        PIC X(8)   VALUE "ENNN044 ".
      * 19. SP-LCMINCHG
           03  FILLER        PIC X(8)   VALUE "ENNN022 ".
      * 20. SP-LCMAXAMT
           03  FILLER        PIC X(8)   VALUE "ENNN042 ".
      * 21. SP-LCGRACE
           03  FILLER        PIC X(8)   VALUE "ENNN020 ".
      * 22. SP-LCYRTYPE
           03  FILLER        PIC X(8)   VALUE "ENNN030 ".
      * 23  SP-LCDELFAC
           03  FILLER        PIC X(8)   VALUE "ENNN030 ".
      * 24. SP-LCUNITPER-CD
           03  FILLER        PIC X(8)   VALUE "E  A010 ".
      * 25. SP-LC-ON-JUDGEMENTS
           03  FILLER        PIC X(8)   VALUE "E  A010 ".
      * 26. SP-AUTO-ASSESS-LCHG
           03  FILLER        PIC X(8)   VALUE "E  A010 ".
      * 27 RENEWAL EDIT
           03  FILLER        PIC X(8)   VALUE "E  A010 ".
      * 28 WRITE INSURANCE
           03  FILLER        PIC X(8)   VALUE "E  A010 ".
      * 29. SP-UNITPER-CD
           03  FILLER        PIC X(8)   VALUE "E  A010 ".

      * 30. SP-DESC
           03  FILLER        PIC X(8)   VALUE "E  A150 ".

       01  SPEC-TAB       REDEFINES SPEC-TABLE.
           03  SPEC-REC   OCCURS 30 TIMES.
               05  SPEC   PIC X(7).
               05  CHFG   PIC X.

       COPY "LIBWI/LCSCANW.CPY".
       COPY "LIBCL/CLMENUW_EXT.CPY".
       COPY "LIBWI/SPSCANW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBWI/MEMOSPW.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBMN/SPMAN1_DEF.CPY".
       COPY "LIBMN/SPMAN1_WKS.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/PASSWDW.CPY".
       COPY "LIBGB/GETFMW.CPY".
       01  FORM-LINK-AREA PIC X(2000).

       SCREEN SECTION.
       COPY "LIBMN/SPMAN1_SCN.CPY".

       PROCEDURE DIVISION
            USING FORM-PATHNAME FORM-LINK-AREA EXIT-PATHNAME.

      *************************************************
      *          MAIN PROGRAM MODULE
      *************************************************
       MAIN-PROGRAM SECTION.
      D    STOP MESS.
           PERFORM SQL-CONNECT.
           MOVE FORM-PATHNAME TO FORM-PATH.

           MOVE "SPMAN1" TO VDU-FORM-NAME.

           MOVE " " TO VDU-WIMENU-SELECTION
                       VDU-PGMENU-SELECTION.

           MOVE FORM-LINK-AREA TO SPMAIN-LINK.

           IF EXT-CONAME-USER-LOC NOT NUMERIC
              MOVE "N" TO BR-INS-ADDON
           ELSE
              MOVE EXT-CONAME-USER-LOC TO BR-NO
              PERFORM OPEN-BR-FILE
              PERFORM READ-BR-FILE
              PERFORM CLOSE-BR-FILE
              IF IO-FG NOT = 0
                 MOVE "N" TO BR-INS-ADDON.

           IF SAVEKEY = " "
              MOVE 1 TO PASSWDW-SEQ
              MOVE "FOR STATE PARAMETER MAINTENANCE" TO PASSWDW-PROMPT2
              PERFORM VERIFY-PASSWORD
              IF PASSWDW-STAT NOT = "OK"
                 MOVE "X" TO ERRCD
                 GO TO END-ROUTINE.

           MOVE EXT-CONAME-SYSDATE TO ALP-DATE.
           PERFORM CONVERT-ALPDATE-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD TO TODAYS-DATE.

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
           IF IO-TYPE = "DEL"
              PERFORM DELETE-MODULE.
           IF NEW-REC = "Y"
              MOVE "P" TO FORM-P1
              MOVE 2 TO FORM-SEL1.
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
           MOVE "N" TO NEW-REC.
           GO TO MAIN-MODULE.

      *****************************************
       REC-SP-MODULE SECTION.
      *****************************************
       REC-SP-MODULE-NO.
           MOVE " " TO IO-TYPE.
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
              MOVE 0        TO SPSCAN-SPRCLASS
              MOVE 0        TO SPSCAN-SUBCLASS
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
              MOVE SP-ORGST     TO SPSCAN-ORGST
              MOVE  SP-SPRCLASS TO SPSCAN-SPRCLASS
              MOVE 0            TO SPSCAN-SUBCLASS
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
           IF VDUSTATUS = "1<"
              MOVE SP-ORGST    TO SPSCAN-ORGST
              MOVE SP-SPRCLASS TO SPSCAN-SPRCLASS
              ADD 1 SP-SUBCLASS GIVING SPSCAN-SUBCLASS
              MOVE 0           TO SPSCAN-LAWCODE
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
           MOVE SP1-KEY TO LPWSSP-SP1-KEY.

           PERFORM OPEN-SP1-FILE.
           PERFORM READ-SP1-FILE.
           PERFORM CLOSE-SP1-FILE.
      * ALL 3 FILES HAD A RECORD WITH THE KEY
           IF IO-FG = 0
              MOVE SP-REC TO ORIGINAL-SP-REC
              PERFORM DISPLAY-FIELDS
              MOVE 0 TO NEW-FLAG
              GO TO END-REC-SP-MODULE.

      * IO-FG NOT 0, YOU ARE HERE!
      * IT WILL BE A NEW RECORD
      * CHECK IF THE IO-MULTI-FG CAME BACK WITH AN ERROR, WHICH WOULD MEAN
      * 1 OF THE SPLIT FILES IS MISSING! AND IF 1 IS MISSING, WE HAVE TO
      * CLEAN ALL THE SPLIT FILES UP SO THIS WILL BE A TOTALLY NEW RECORD

             
           IF IO-MULTI-FG NOT = SPACES
      * DONT WANT TO DISPLAY THE MESSAGE ON A NORMAL MISSING NEW RECORD
              IF IO-MULTI-FG NOT = "A"
                 MOVE SPACES TO MESS
                 STRING "WARNING: MISSING RECORD IN ",IO-MULTI-FG 
                      DELIMITED BY "  " INTO MESS
                 PERFORM SEND-MESS
              END-IF
              PERFORM CLEAN-MULTI-FILES.

      * NOW YOU ARE BACK HERE WITH NO RECORDS EXISTING IN ANY OF THE MULTI
      * PART FILES

           MOVE 0 TO SP-LAWCODE HOLD-EXPDATE.

       VERIFY-EXPIRED-RECS.

           ADD 1 TO SP-LAWCODE.

           PERFORM OPEN-SP1-FILE.
           PERFORM READ-SP1-FILE.
           PERFORM CLOSE-SP1-FILE.
           IF IO-FG NOT = 0
              IF LPWSSP-LAWCODE = 0
                 MOVE "SN N020SPCLASS." TO VDU
                 MOVE SP-LAWCODE TO LPWSSP-LAWCODE VDUNUM0
                 PERFORM SCREENIO
                 GO TO REC-SP-EFFDATE.

           IF IO-FG NOT = 0
              IF SP-LAWCODE = LPWSSP-LAWCODE
                 GO TO REC-SP-EFFDATE
              ELSE
                 MOVE " PRESS 'RETURN' FOR NEXT AVAIL. NUMBER" TO MESS
                 PERFORM SEND-MESS
                 GO TO REC-SP-LAWCODE.

           IF SP-EXPDATE NOT = 0
              MOVE SP-EXPDATE TO HOLD-EXPDATE
              GO TO VERIFY-EXPIRED-RECS.

           MOVE " PRIOR SPR STILL IN EFFECT, VIEW IT ?" TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "R RA060SEL." TO VDU.
           PERFORM SCREENIO.
           IF VDUBUF = "     Y"
              MOVE "SN N020SPCLASS." TO VDU
              MOVE SP-LAWCODE        TO VDUNUM0
              PERFORM SCREENIO
              MOVE " " TO MESS
              PERFORM SEND-LEGEND
              GO TO READ-SPFILE.

           MOVE " " TO MESS.
           PERFORM SEND-LEGEND.

           GO TO REC-SP-LAWCODE.
       REC-SP-EFFDATE.
           MOVE "ERNM060EFFDATE." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10"
              GO TO REC-SP-MODULE-NO.
           IF VDUSTATUS = "1U"
              GO TO REC-SP-LAWCODE.
           IF VDUSTATUS NOT = "00"
              GO TO REC-SP-EFFDATE.
           MOVE VDU-DATE-YYYYMMDD TO HOLD-EFFDATE.

           IF HOLD-EFFDATE = 0
              IF HOLD-EXPDATE = 0
                 GO TO REC-SP-EFFDATE
              ELSE
                 MOVE "S  A080EFFDATE." TO VDU
                 MOVE HOLD-EXPDATE TO HOLD-EFFDATE DATE-YYYYMMDD
                 PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
                 MOVE DATE-MMDDYY TO VDU-DATE-EDIT
                 PERFORM SCREENIO.
           IF HOLD-EXPDATE NOT = 0
              IF HOLD-EFFDATE NOT = HOLD-EXPDATE
                 MOVE " INVALID DATE" TO MESS
                 PERFORM SEND-MESS
                 GO TO REC-SP-EFFDATE.

           PERFORM NEW-RECORD-SETUP.
       END-REC-SP-MODULE.
           EXIT.

      *******************************************
       CLEAN-MULTI-FILES SECTION.
      *******************************************

      * IF IO-MULTI-FG = "A", FIRST FILE MISSING KEY READ, WHICH IS WHAT
      * YOU WOULD NORMALLY EXPECT FOR A NEW RECORD, BUT NEED TO MAKE
      * SURE NONE OF THE OTHERS EXIST EITHER.

           MOVE LPWSSP-SP1-KEY TO SP1-KEY.
           IF IO-MULTI-FG = "A"
              PERFORM CLEAN-FILE-B
              PERFORM CLEAN-FILE-C
           ELSE
      * IF "B", THEN PRIMARY FILE 'A' HAD A RECORD AND "B" DID NOT
      * SHOULD NOT HAPPEN BUT HAVE TO FIX IF IT DOES
              IF IO-MULTI-FG = "B"
                 PERFORM CLEAN-FILE-A
                 PERFORM CLEAN-FILE-C
              ELSE
      * IF "C", THEN FILE 'A' AND 'B' HAD A RECORD AND "C" DID NOT
      * SHOULD NOT HAPPEN BUT HAVE TO FIX IF IT DOES
              IF IO-MULTI-FG = "C"
                 PERFORM CLEAN-FILE-A
                 PERFORM CLEAN-FILE-B.
       EXIT-CLEAN-MULTI-FILES.
           EXIT.

      *******************************************
       CLASS-SCAN SECTION.
      *******************************************
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

      ********************************************
       ENTER-SCANKEY SECTION.
      ********************************************
           MOVE SPACES TO SP-ORGST.
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
           IF VDUBUF = " "
              IF FIRST-SCAN NOT = 0
                 MOVE SP1-KEY TO LPWSSP-SP1-KEY
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
           MOVE " " TO MESS.
           PERFORM CALL-TMMSG.
       END-SCANKEY.
           EXIT.

      *************************************************
      *   ENTRY-MODULE
      *   NEW-FLAG = 0 - MODIFY EXISTING RECORD
      *              1 - INITIAL NEW RECORD ENTRY
      *              2 - MODIFY NEW RECORD
      ************************************************
       ENTRY-MODULE SECTION.
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
      *    PERFORM CHECK-PDTH-PCTS.
           MOVE "R RA060SEL." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF F2-KEY
              MOVE SP1-KEY      TO MEMOSP-SP1-KEY
              MOVE "WI/MEMOSP" TO FORM-NAM
              CALL FORM-PROGX USING FORM-PATH MEMOSP-BUF EXIT-PATHNAME
              CANCEL FORM-PROGX
              GO TO MODIFY-ROUTINE.
           IF VDUSTATUS NOT = "00"
              GO TO MODIFY-ROUTINE.
           MOVE VDUBUF TO ACTION.

           IF ACTION = "353635" AND NEW-FLAG = 0
              MOVE "R RA060SEL." TO VDU
              PERFORM SCREENIO
              MOVE VDUBUF TO ACTION
              IF ACTION = "363536"
                 MOVE
                 "EFFECT DATE CHANGE, YOUR ON YOUR OWN, F1-ABORT"
                                                            TO MESS
                 PERFORM SEND-MESS
                 MOVE "ERNM060EFFDATE." TO VDU
                 PERFORM SCREENIO
                 IF VDUSTATUS = "10"
                    MOVE "ABORTING" TO MESS
                    PERFORM SEND-MESS
                    MOVE "CLR" TO IO-TYPE
                    GO TO END-ENTRY-MODULE
                 END-IF
                 IF VDUNUM0 = 0
                    MOVE "INVALID ENTRY, ABORTING" TO MESS
                    PERFORM SEND-MESS
                    MOVE "CLR" TO IO-TYPE
                    GO TO END-ENTRY-MODULE
                 END-IF
                 MOVE VDU-DATE-YYYYMMDD TO SP-EFFDATE
                 MOVE "NEW EFFECTIVE DATE WILL BE SET WITH WRITE"
                                                            TO MESS
                 PERFORM SEND-MESS.

      * TO DELETE;
           IF ACTION = "  3535" OR "3535  "
              MOVE "R RA060SEL." TO VDU
              PERFORM SCREENIO
              MOVE VDUBUF TO ACTION
              IF ACTION = "DELETE"
                 MOVE "R RA060SEL." TO VDU
                 PERFORM SCREENIO
                 MOVE VDUBUF TO ACTION
                 IF ACTION = "  3535" OR "3535  "
                    MOVE "DEL" TO IO-TYPE
                    GO TO END-ENTRY-MODULE
                 ELSE
                    MOVE SPACES TO ACTION
                    GO TO MODIFY-ROUTINE
              ELSE
                 MOVE SPACES TO ACTION
                 GO TO MODIFY-ROUTINE.

           IF ACTION = " "
              PERFORM GET-NOTES
              MOVE "RWR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF ACTION = "DELETE"
              MOVE "DEL" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
      * FORM-NUM1 CHECKS FOR PAGES 1 THRU 20
           IF FORM-KEY1 = " " AND FORM-P1 = "P" AND FORM-SEL1 NUMERIC
              IF FORM-SEL1 < 2 OR FORM-SEL1 = 1
                 GO TO MODIFY-ROUTINE
              ELSE
                 GO TO CHECK-PAGE.
      * FORM-NUM2 CHECKS FOR PAGE 14
           IF FORM-KEY2 = " " AND FORM-P2 = "P" AND FORM-SEL2 NUMERIC
              IF NOT FORM-2-DIGITS
                 GO TO MODIFY-ROUTINE
              ELSE
                 GO TO CHECK-PAGE.
           GO TO CHECK-CHNO.
       CHECK-PAGE.
           MOVE "RWR" TO IO-TYPE.
           GO TO EXIT-ENTRY-MODULE.
       CHECK-CHNO.
           INSPECT ACTION REPLACING ALL " "
              BY ZEROS.
           IF CHNO-X NOT NUMERIC
              GO TO MODIFY-ROUTINE.
           IF CHNO = 0
              GO TO MODIFY-ROUTINE.
           IF CHNO > MAX
              GO TO MODIFY-ROUTINE.
           MOVE CHNO TO ELE.
       ENTER-ELE.
           IF BR-UNITPER-FG NOT = "Y"
              IF ELE = 8 OR 11 OR 24
                 GO TO CREATE-ROUTINE.
           IF CHKEY NOT = ZEROS
              IF CHFG(ELE) = " "
                 IF NEW-FLAG NOT = 1
      * ADDED TO ALLOW LORI TO CHANGE EXPDATES AT REGWAN BY USING 010101
                    IF ELE = 1 AND CHKEY = 0101
                       NEXT SENTENCE
                    ELSE
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
              GO TO END-ENTRY-MODULE.
           IF VDUSTATUS NOT = "00"
              GO TO ENTER-ELE.
           MOVE 1 TO ONE.
       GOTO-FIELD.
           GO TO A-01 A-02 A-03 A-04 A-05
                 A-06 A-07 A-08 A-09 A-10
                 A-11 A-12 A-13 A-14 A-15
                 A-16 A-17 A-18 A-19 A-20
                 A-21 A-22 A-23 A-24 A-25
                 A-26 A-27 A-28 A-29 A-30
                 DEPENDING ON ELE.
           GO TO END-ENTRY-MODULE.

      *    **********************************
      *    *      SET RECORD ELEMENTS
      *    **********************************
       A-01.
           IF ACTION NOT = "010101"
            IF SP-EXPDATE NOT = 0 AND 
                            VDU-DATE-YYYYMMDD NOT = SP-EXPDATE
              PERFORM TEST-FOR-NEW-SPR
              IF EXPDATE-CHANGE-FG = "N"
                 MOVE "CANNOT CHANGE SPR EXPIRATION DATE" TO MESS
                 PERFORM SEND-MESS
                 MOVE "A NEW LAWCODE EXISTS FOR THIS SPR" TO MESS
                 PERFORM SEND-MESS
                 MOVE SPACES TO MESS
                 STRING "NEW LAWCODE MUST BE DELETED BEFORE"
                        " DATE CAN BE CHANGED"
                        DELIMITED BY SIZE INTO MESS
                 PERFORM SEND-MESS
      *          --------------
      *          REDISPLAY DATE
      *          --------------
                 MOVE SPEC(ELE) TO VDU
                 MOVE ELE TO VDUROW
                 MOVE 0 TO VDUCOL
                 MOVE "S" TO VDUACTION
                 MOVE " " TO VDUPUNC VDURESET
                 MOVE "A" TO VDUCLASS
                 MOVE SP-EXPDATE TO DATE-YYYYMMDD
                 PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
                 MOVE DATE-MMDDYY TO VDU-DATE-EDIT
                 MOVE "." TO VDUDECIMAL
                 PERFORM SCREENIO
                 GO TO ENTER-ELE.

           MOVE VDU-DATE-YYYYMMDD TO SP-EXPDATE.
           GO TO CREATE-ROUTINE.
       A-02.
           MOVE VDUBUF TO SP-JUDGFRMLA.
           IF NOT SP-JUDGFRMLA-VALID
              MOVE "ENTER: SPACE, 'A' OR 'B'" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
       A-021.
           MOVE "ENNN033021." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF VDUSTATUS NOT = "00" AND NOT = "1D"
              GO TO A-021.
           MOVE VDUNUM3 TO SP-JUDGRATE.
       A-022.
           MOVE "ENNN030022." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO A-021.
           IF VDUSTATUS NOT = "00" AND NOT = "1D"
              GO TO A-022.
           MOVE VDUNUM0 TO SP-JUDGYRTYPE.
           IF NOT SP-JUDGYRTYPE-VALID
              MOVE "ENTER: 358, 360, 365, 367 OR 999" TO MESS
              PERFORM SEND-MESS
              GO TO A-022.
       A-023.
           MOVE "E  A010023." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO A-022.
           IF VDUSTATUS NOT = "00" AND NOT = "1D"
              GO TO A-023.
           MOVE VDUBUF TO SP-JUDGINTMETH.
           IF NOT SP-JUDGINTMETH-VALID
              MOVE "ENTER: 'E' OR 'O'" TO MESS
              PERFORM SEND-MESS
              GO TO A-023.
           GO TO CREATE-ROUTINE.
       A-03.
           MOVE VDUBUF TO SP-1098FRMLA.
           IF NOT SP-1098FRMLA-VALID
              MOVE "ENTER: SPACE, 'A', 'B', 'C' OR 'D'" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           IF LC-LOANTYPE = "I"
              IF SP-1098FRMLA NOT = " "
                 MOVE "IB ACCOUNTS MUST BE A SPACE" TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-ELE.
           IF LC-LOANTYPE = "P"
              IF SP-1098FRMLA = " "
                 MOVE "PC ACCOUNTS CANNOT BE A SPACE" TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-ELE.
           IF LC-SERVACCT = "Y"
              IF SP-1098FRMLA = "B"
                 MOVE "SERVICE ACCOUNTS CAN NOT BE A 'B'" TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-ELE.
           IF BR-INS-ADDON = "Y" OR "O"
              IF LC-LOANTYPE = "P"
                 IF NOT (SP-1098FRMLA = "A" OR "B")
                     MOVE "ADDON CLASSES MUST BE 'A' OR 'B'" TO MESS
                     PERFORM SEND-MESS
                     GO TO ENTER-ELE.
           PERFORM VALIDATE-1098FRMLA.
       A-032.
           MOVE "E  A010032." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO A-032.
           MOVE VDUBUF TO SP-1098-INC-LC.
           IF SP-1098-INC-LC NOT = "Y" AND NOT = "N"
              MOVE "MUST BE 'Y' OR 'N'" TO MESS
              PERFORM SEND-MESS
              GO TO A-032.
           GO TO CREATE-ROUTINE.
       A-04.
           MOVE VDUBUF TO SP-DISFRMLA.
           IF NOT SP-DISFRMLA-VALID
              MOVE "ENTER: 'A', 'B' OR 'C'" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           PERFORM VALIDATE-DISFRMLA.
       A-041.
           MOVE "E  A010041." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO A-041.
           MOVE VDUBUF TO SP-DISFRMLA-SPAY-FG.
           IF NOT (SP-DISFRMLA-SPAY-FG = " " OR "N" OR "Y")
              MOVE "ENTER: ' ' OR 'Y' FOR SINGLE PAY (365) "
                                                      TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       A-05.
           MOVE VDUBUF TO SP-CONTRFRMLA.
           IF NOT SP-CONTRFRMLA-VALID
              MOVE "ENTER: SPACE OR 'A'" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           IF SP-CAL-RATETYPE(1) = "Z" AND
              SP-CONTRFRMLA = "A"
              STRING "TOTAL MONEY FORMULA NOT ALLOWED WITH "
                     "RATETYPE 'Z' INT METHOD"
                     DELIMITED BY SIZE INTO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           PERFORM VALIDATE-CONTRACTUAL.
           GO TO CREATE-ROUTINE.
       A-06.
           MOVE VDUBUF TO SP-BANKRULE.
           IF NOT SP-BANKRULE-VALID
              MOVE "ENTER: SPACE, 'A', 'B', 'C' OR 'D' OR 'F'" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           PERFORM VALIDATE-BANKRULE.
           IF NOT (SP-BANKRULE = "E")
              MOVE "S  A030071." TO VDU
              MOVE SPACES TO VDUBUF
              MOVE 0 TO SP-BANKRULE-RATE
              PERFORM SCREENIO.
           GO TO CREATE-ROUTINE.
       A-07.
           MOVE VDUNUM0 TO SP-CONTPDTH.
           IF NOT SP-CONTPDTH-VALID
              MOVE "ENTER: 0 - 100" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
       A-071.
           IF NOT (SP-BANKRULE = "E")
              MOVE "SNNN030071." TO VDU
              MOVE 0 TO VDUNUM0 SP-BANKRULE-RATE
              PERFORM SCREENIO
              GO TO CREATE-ROUTINE.
           MOVE "ENNN030071." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF VDUSTATUS NOT = "00" AND NOT = "1D"
              GO TO A-071.
           MOVE VDUNUM0 TO SP-BANKRULE-RATE.
           IF NOT SP-BANKRULE-RATE-VALID
              MOVE "ENTER: 0 - 100" TO MESS
              PERFORM SEND-MESS
              GO TO A-071.
           GO TO CREATE-ROUTINE.
       A-08.
           MOVE VDUBUF TO SP-BANKUNITPER-CD.
           IF NOT (SP-BANKUNITPER-CD-VALID)
              MOVE "ENTER: SPACE, 'A' OR 'B' OR 'C' OR 'D' OR 'E' "
                                                              TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       A-09.
           MOVE VDUBUF TO SP-TRWBANKRULE.
           IF NOT SP-TRWBANKRULE-VALID
              MOVE "ENTER: SPACE, 'A', 'B', 'C' OR 'D' OR 'F'" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           PERFORM VALIDATE-TRWBANKRULE.
           GO TO CREATE-ROUTINE.
       A-10.
           MOVE VDUNUM0 TO SP-TRWCONTPDTH.
           IF NOT SP-TRWCONTPDTH-VALID
              MOVE "ENTER: 0 - 100" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       A-11.
           MOVE VDUBUF TO SP-TRWBANKUNITPER-CD.
           IF NOT (SP-TRWBANKUNITPER-CD-VALID)
              MOVE "ENTER: SPACE, 'A' OR 'B' OR 'C' OR 'D' OR 'E'"
                                                           TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       A-12.
           MOVE VDUBUF TO SP-RECFRMLA.
           IF NOT SP-RECFRMLA-VALID
              MOVE "ENTER: ' ', 'C' 'D' OR 'P'" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           PERFORM VALIDATE-RECFRMLA.
           IF SP-RECFRMLA NOT = "P"
              MOVE"S  A010  1." TO VDU
              MOVE ELE TO VDUROW
              MOVE " " TO SP-RECPAYCD VDUBUF
              PERFORM SCREENIO
              MOVE"S  A010  1D." TO VDU
              MOVE ELE TO VDUROW
              MOVE " " TO VDUBUF
              PERFORM SCREENIO
              MOVE"S  A030  2." TO VDU
              MOVE ELE TO VDUROW
              MOVE 0 TO SP-RECPDTH
              MOVE " " TO VDUBUF
              PERFORM SCREENIO
              MOVE"S  A030  2D." TO VDU
              MOVE ELE TO VDUROW
              MOVE " " TO VDUBUF
              PERFORM SCREENIO
              GO TO CREATE-ROUTINE.
       A-121.
           MOVE"E  A010  1." TO VDU.
           MOVE ELE TO VDUROW.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO A-121.
           MOVE VDUBUF TO SP-RECPAYCD.
           IF NOT SP-RECPAYCD-VALID
              MOVE "ENTER ' ' OR 'A'" TO MESS
              PERFORM SEND-MESS
              GO TO A-121.
           PERFORM VALIDATE-RECPAYCD.
       A-122.
           MOVE"ENNR030  2." TO VDU.
           MOVE ELE TO VDUROW.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO A-121.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO A-122.
           MOVE VDUNUM0 TO SP-RECPDTH.
           IF NOT SP-RECPDTH-VALID
              MOVE "ENTER 0 TRHU 100" TO MESS
              PERFORM SEND-MESS
              GO TO A-121.
           PERFORM VALIDATE-RECPDTH.
           GO TO CREATE-ROUTINE.
       A-13.
           MOVE VDUBUF TO SP-RESCIND-BUS-DAYS.
           IF NOT SP-RESCIND-BUS-DAYS-VALID
              MOVE "ENTER: 0 THRU 8 ONLY" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       A-14.
           MOVE VDUBUF TO SP-RESCIND-SATURDAY.
           IF NOT SP-RESCIND-SATURDAY-VALID
              MOVE "ENTER: 'Y' OR 'N' ONLY" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       A-15.
           MOVE VDUBUF TO SP-CAN-LOANS-FG.

           IF NOT ( SP-CAN-LOANS-VALID )
              MOVE "ENTER: (Y)ES OR (N)O OR (O)NLY" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.

           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       A-16.
           MOVE VDUBUF TO SP-LCFRMLA.

           IF NOT SP-LCFRMLA-VALID
              MOVE "ENTER: SPACE, 'A' - 'N', 'P', 'T' OR 'X'"
                                                      TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.

           IF LC-LOANTYPE = "I"
              IF NOT
                 (SP-LCFRMLA = " " OR "B" OR "G" OR "J" OR "K" OR "D"
                                   OR "E" OR "F" OR "H" OR "I" OR "L"
                                   OR "M" OR "N" OR "P" OR "S" OR "X"
                                   OR "T")
                  MOVE
           "IB CLASSES: ' ',B,J,G,K,D,E,F,H,I,L,M,N,P,S,T,X" TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-ELE.
           IF NOT (SP-LCFRMLA = " " OR "B" OR "D" OR "G" OR "K")
              IF SP-LCTYPE = "M"
                 MOVE
           "MUST USE LCFRMLA 'B' OR 'D' OR 'G' OR 'K' WITH LC CODE 'M'"
                                                         TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-ELE.
           IF SP-CAL-RATETYPE(1) = "Z" AND
              SP-LCFRMLA-TOTAL-MONEY
              STRING "TOTAL MONEY FORMULA NOT ALLOWED WITH "
                     "RATETYPE 'Z' INT METHOD"
                     DELIMITED BY SIZE INTO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       A-17.
           MOVE VDUBUF TO SP-LCTYPE.
           IF NOT SP-LCTYPE-VALID
              MOVE "ENTER: SPACE, OR 'A' THRU 'Q' " TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           IF SP-LCFRMLA = "A"
              IF SP-LCTYPE NOT = "D"
                 MOVE "MUST BE 'D' IF LC FORMULA IS AN 'A'" TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-ELE.
           IF SP-LCFRMLA = "B" OR "C" OR "D" OR "J" OR "N"
              IF SP-LCTYPE = "D"
                 MOVE "CAN NOT BE 'D' IF LC FORMULA IS: B,C,D,J,N"
                                                            TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-ELE.
           IF SP-LCTYPE = "M"
              IF NOT (SP-LCFRMLA = " " OR "B" OR "D" OR "G" OR "K")
                 MOVE "MUST HAVE LCFRMLA 'B' OR 'D' OR 'G' OR 'K'"
                                                             TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-ELE.
           PERFORM SEND-LCTYPE-DESC.
           GO TO CREATE-ROUTINE.
       A-18.
           MOVE VDUNUM4 TO SP-LCRATE.
           GO TO CREATE-ROUTINE.
       A-19.
           MOVE VDUNUM2 TO SP-LCMINCHG.
           GO TO CREATE-ROUTINE.
       A-20.
           MOVE VDUNUM2 TO SP-LCMAXAMT.
           GO TO CREATE-ROUTINE.
       A-21.
           MOVE VDUNUM0 TO SP-LCGRACE.
       A-211.
           MOVE "E  A010211." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO A-211.
           MOVE VDUBUF TO SP-LCGRACECD.
           IF NOT SP-LCGRACECD-VALID
              MOVE "ENTER: SPACE, S'UNDAY', OR W'EEKEND'" TO MESS
              PERFORM SEND-MESS
              GO TO A-211.
           GO TO CREATE-ROUTINE.
       A-22.
           MOVE VDUNUM0 TO SP-LCYRTYPE.
           IF SP-LCYRTYPE = 0
              IF SP-LCFRMLA = " "
                 GO TO CREATE-ROUTINE.
           IF NOT SP-LCYRTYPE-VALID
              MOVE "ENTER: 358, 360, 365, OR 367" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       A-23.
           MOVE VDUNUM0 TO SP-LCDELFAC.
           IF NOT SP-LCDELFAC-VALID
              MOVE "ENTER: 0 - 100" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       A-24.
           MOVE "ENTER LATE CHARGE PAY FREQUENCY CODE" TO MESS.
           PERFORM SEND-MESS.
           MOVE "E  A010240." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO A-24.
           MOVE VDUBUF TO SP-LCUNITPER-CD.
           IF NOT (SP-LCUNITPER-CD = " " OR "A")
              MOVE "ENTER: SPACE OR 'A'" TO MESS
              PERFORM SEND-MESS
              GO TO A-24.
           GO TO CREATE-ROUTINE.
       A-25.
           MOVE VDUBUF TO SP-LC-ON-JUDGEMENTS.
           IF SP-LC-ON-JUDGEMENTS NOT = "Y" AND NOT = "N"
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       A-26.
           MOVE VDUBUF TO SP-AUTO-ASSESS-LCHG.
           IF SP-AUTO-ASSESS-LCHG NOT = "Y" AND NOT = "N"
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       A-27.
           MOVE VDUBUF TO SP-RENEWAL-EDIT.
           IF NOT (SP-RENEWAL-EDIT-VALID)
              MOVE " ENTER ' ', 'A', 'B', 'C', 'D' OR 'E'" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       A-28.
           MOVE VDUBUF TO SP-INSURANCE-WRITTEN.
           IF NOT(SP-INSURANCE-WRITTEN = "Y" OR "N")
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       A-29.
           MOVE VDUBUF TO SP-UNITPER-CD.
           IF NOT (SP-UNITPER-CD-VALID)
              MOVE " ENTER ' ' OR 'M' OR 'S' OR 'B' OR 'W' " TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.

           GO TO CREATE-ROUTINE.
       A-30.
           MOVE VDUBUF TO SP-DESC.
           PERFORM DISPLAY-SUBCLASS-DESC.
           GO TO CREATE-ROUTINE.

       END-ENTRY-MODULE.
           MOVE 0 TO FORM-SEL1 FORM-SEL2.
       EXIT-ENTRY-MODULE.
           EXIT.

      *******************************
       VERIFY-PASSWORD SECTION.
      *******************************
           MOVE " " TO PASSWDW-STAT.
           MOVE "SPMAN1" TO PASSWDW-PGM.
           MOVE "PLEASE ENTER THE PASSWORD," TO PASSWDW-PROMPT1.
           MOVE "CL/PASSWD" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH PASSWDW-BUF.
           CANCEL FORM-PROGX.


      ******************************************
       GET-NOTES SECTION.
      ******************************************
           MOVE "E  A600NOTES." TO VDU.
           PERFORM SCREENIO.
           IF F2-KEY
              MOVE SP1-KEY      TO MEMOSP-SP1-KEY
              MOVE "WI/MEMOSP" TO FORM-NAM
              CALL FORM-PROGX USING FORM-PATH MEMOSP-BUF EXIT-PATHNAME
              CANCEL FORM-PROGX
              GO TO GET-NOTES.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO GET-NOTES.
           MOVE VDUBUF TO SP-NOTES.

       GET-NOTES-EXIT.
           EXIT.

      ******************************************
       WRITE-MODULE SECTION.
      ******************************************
           PERFORM OPEN-SP1-FILE.
           IF NEW-FLAG NOT = 0
              GO TO WRITE-RECORD.
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

           MOVE 6 TO SP-FINCHG.
           PERFORM REWRITE-SP1-FILE.
      *WRITES A SPR MEMO WHEN CHANGES OCCUR
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

       WRITE-RECORD.
           MOVE 6 TO SP-FINCHG.
           PERFORM WRITE-SP1-FILE
           IF IO-FG NOT = 0
              PERFORM ALREADY-CREATED
           ELSE
              MOVE "Y" TO NEW-REC.
       END-WRITE-MODULE.
           PERFORM CLOSE-SP1-FILE.

      ******************************************
       DELETE-MODULE SECTION.
      ******************************************
           IF NEW-FLAG NOT = 0
              GO TO END-DELETE-MODULE.

           PERFORM OPEN-SP1-FILE.
           PERFORM READ-SP1-FILE.
           IF IO-FG NOT = 0
              PERFORM UNDERGONE-CHANGE
              PERFORM RECORD-DELETED
              GO TO END-DELETE-MODULE.
           IF SP-REC NOT = ORIGINAL-SP-REC
              PERFORM UNDERGONE-CHANGE
              PERFORM RECORD-CHANGED
              GO TO END-DELETE-MODULE.

           MOVE SP-EXPDATE TO NUM-DATE.
           MOVE TODAYS-DATE TO SYS-DATE.
           PERFORM TIM.
           IF ELAPSED-DAYS < 0
               PERFORM NOT-EXPIRED
               GO TO END-DELETE-MODULE.

           PERFORM DELETE-SP1-FILE.

      *WRITE CHANGES FILE
           MOVE "C" TO ERRLOGW-ACTION.
           MOVE " " TO ERRLOGW-KEY.
           STRING SP1-KEY, " ", "DELETED SPR" DELIMITED BY SIZE
                INTO ERRLOGW-KEY.

           MOVE FORM-PATHNAME TO ERRLOGW-PROGX.

           MOVE "CL/ERRLOG" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH ERRLOGW-AREA.
           CANCEL FORM-PROGX.

       END-DELETE-MODULE.
           PERFORM CLOSE-SP1-FILE.

      ******************************************
       SPSCAN-SCAN SECTION.
      ******************************************
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

      ******************************************
       NEW-RECORD-SETUP SECTION.
      ******************************************

           MOVE 1 TO NEW-FLAG.
           MOVE "S  A000NEW."        TO VDU.
           MOVE "*** NEW RECORD ***" TO VDUBUF.
           PERFORM SCREENIO.

           PERFORM CLEAR-SP-FILE.

       NEW-RECORD-EXIT.
           EXIT.

      ******************************************
       DISPLAY-FIELDS SECTION.
      ******************************************
           PERFORM DISPLAY-SUBCLASS-DESC.
           MOVE " " TO DISPLAY-BUF.
           MOVE SP-NOTES TO NOTES.
           MOVE "S  A080EFFDATE." TO VDU.
           MOVE SP-EFFDATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO VDU-DATE-EDIT.
           PERFORM SCREENIO.
           MOVE SP-EXPDATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO A01.
           MOVE SP-JUDGFRMLA TO A02.
           MOVE SP-JUDGRATE TO A021.
           MOVE SP-JUDGYRTYPE TO A022.
           MOVE SP-JUDGINTMETH TO A023.
           MOVE SP-1098FRMLA TO A03.
           MOVE SP-1098-INC-LC TO A032.
           MOVE SP-DISFRMLA TO A04.
           MOVE SP-DISFRMLA-SPAY-FG TO A041.
           MOVE SP-CONTRFRMLA TO A05.
           PERFORM VALIDATE-CONTRACTUAL.
           MOVE SP-BANKRULE TO A06.
           PERFORM VALIDATE-BANKRULE.
           MOVE SP-CONTPDTH TO A07.
           IF SP-BANKRULE = "E"
              MOVE SP-BANKRULE-RATE TO A071
           ELSE
              MOVE 0 TO A071.
           MOVE SP-BANKUNITPER-CD TO A08.
           MOVE SP-TRWBANKRULE TO A09.
           PERFORM VALIDATE-TRWBANKRULE.
           MOVE SP-TRWCONTPDTH TO A10.
           MOVE SP-TRWBANKUNITPER-CD TO A11.
           MOVE SP-RECFRMLA TO A12.
           MOVE SP-RECPAYCD TO A121.
           MOVE SP-RECPDTH  TO A122.
           MOVE SP-RESCIND-BUS-DAYS TO A13.
           MOVE SP-RESCIND-SATURDAY TO A14.
           MOVE SP-CAN-LOANS-FG TO A15.
           MOVE SP-LCFRMLA TO A16.
           MOVE SP-LCTYPE TO A17.
           MOVE SP-LCRATE TO A18.
           MOVE SP-LCMINCHG TO A19.
           MOVE SP-LCMAXAMT TO A20.
           MOVE SP-LCGRACE TO A21.
           MOVE SP-LCGRACECD TO A211.
           MOVE SP-LCYRTYPE TO A22.
           MOVE SP-LCDELFAC TO A23.
           MOVE SP-LCUNITPER-CD TO A24.
           MOVE SP-LC-ON-JUDGEMENTS TO A25.
           MOVE SP-AUTO-ASSESS-LCHG TO A26.
           MOVE SP-RENEWAL-EDIT TO A27.
           MOVE SP-INSURANCE-WRITTEN TO A28.
           MOVE SP-UNITPER-CD        TO A29.
           MOVE SP-DESC TO A30.
           MOVE DISPLAY-BUF TO WR-BUF.
           MOVE DISPLAY-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

           PERFORM VALIDATE-1098FRMLA.
           PERFORM VALIDATE-DISFRMLA.
           PERFORM VALIDATE-RECFRMLA.
           PERFORM VALIDATE-RECPAYCD.
           PERFORM VALIDATE-RECPDTH.
           PERFORM SEND-LCTYPE-DESC.
       DISPLAY-FIELDS-EXIT.
           EXIT.

       COPY "LIBGB/DATER.CPY".
       COPY "LIBLP/LPTBNO.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBLP/LPSPCL.CPY".

      ******************************************************************
       TEST-FOR-NEW-SPR SECTION.
      ******************************************************************

      * UPON CHANGEING EXPIRATION DATE FOR AN SPR, CONFIRM THAT THERE
      * IS NOT ALREADY A NEW RECORD OUT THERE WAITING TO BECOME
      * EFFECTIVE WHEN THIS ONE EXPIRES.

      * USED CHANGED-REC HERE TO SAVE ON OVERHEAD, IT IS ONLY USED
      * ON REWRITE.

           MOVE SP-REC TO CHANGED-SP-REC
           MOVE SP1-KEY TO TEST-SP1-KEY.
           MOVE 99 TO SP-LAWCODE.
           PERFORM OPEN-SP1-FILE.

           PERFORM START-SP1-FILE-NOT-GREATER.
           PERFORM READ-SP1-FILE-PREVIOUS.
           IF IO-FG = 0 AND SP-ORGST = TEST-SP-ORGST
                        AND SP-SPRCLASS = TEST-SP-SPRCLASS
                        AND SP-SUBCLASS = TEST-SP-SUBCLASS
                        AND SP-LAWCODE > TEST-SP-LAWCODE
              MOVE "N" TO EXPDATE-CHANGE-FG
           ELSE
              MOVE "Y" TO EXPDATE-CHANGE-FG.
           MOVE CHANGED-SP-REC TO SP-REC.
           PERFORM CLOSE-SP1-FILE.

       TEST-FOR-NEW-SPR-EXIT.
           EXIT.

      ******************************************
       FORM-RESET SECTION.
      ******************************************
           MOVE EXT-CONAME-CONAME TO SPMAN1-CONAME.
           MOVE EXT-CONAME-SYSDATE TO SPMAN1-SYSDATE.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN
           DISPLAY SPMAN1-SCREEN UPON SPMAIN-WINDOW-HANDLE.
           MOVE SPMAN1-FORM-FIELDS TO WR-BUF.
           MOVE SPMAN1-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      ******************************************
       SETUP-LINK-INFO SECTION.
      ******************************************
           MOVE SPMAN1-HELP--FILE TO HELP-LINK-FILE.
           MOVE SPMAN1-HELP--DIR TO HELP-LINK-DIR.

      ******************************************
       VDU-CONVERT-FIELD SECTION.
      ******************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBMN/SPMAN1_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ******************************************
       MISC-PARAGRAPHS SECTION.
      ******************************************
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/GETENV.CPY".
       CLEAN-FILE-A.
           PERFORM OPEN-SPA1-FILE.
           PERFORM READ-SPA1-FILE.
           IF IO-FG = 0
              PERFORM DELETE-SPA1-FILE.
           PERFORM CLOSE-SPA1-FILE.
       CLEAN-FILE-B.
           PERFORM OPEN-SPB1-FILE.
           PERFORM READ-SPB1-FILE.
           IF IO-FG = 0
              PERFORM DELETE-SPB1-FILE.
           PERFORM CLOSE-SPB1-FILE.
       CLEAN-FILE-C.
           PERFORM OPEN-SPC1-FILE.
           PERFORM READ-SPC1-FILE.
           IF IO-FG = 0
              PERFORM DELETE-SPC1-FILE.
           PERFORM CLOSE-SPC1-FILE.
       SEND-LEGEND.
           MOVE "S  A000MSEL." TO VDU.
           MOVE MESS TO VDUBUF.
           PERFORM SCREENIO.

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
       VALIDATE-TRWBANKRULE.
           MOVE "S  A130091." TO VDU.
           MOVE "INVALID" TO VDUBUF.
           IF SP-BANKRULE = " "
              MOVE "30DAYS / 2 MO" TO VDUBUF
           ELSE
           IF SP-TRWBANKRULE = "A"
              MOVE "30/1ST 2ND MO" TO VDUBUF
           ELSE
           IF SP-TRWBANKRULE = "B"
              MOVE "30 AFTER 1 MO" TO VDUBUF
           ELSE
           IF SP-TRWBANKRULE = "C"
              MOVE "30/1ST MO FOL" TO VDUBUF
           ELSE
           IF SP-TRWBANKRULE = "D"
              MOVE "WRLD/1ST FOL" TO VDUBUF
           ELSE
           IF SP-TRWBANKRULE = "F"
              MOVE "WRLD/SEMI-MONTHLY" TO VDUBUF.
           PERFORM SCREENIO.
       VALIDATE-BANKRULE.
           MOVE "S  A130061." TO VDU.
           MOVE "INVALID" TO VDUBUF.
           IF SP-BANKRULE = " "
              MOVE "30DAYS / 2 MO" TO VDUBUF
           ELSE
           IF SP-BANKRULE = "A"
              MOVE "30/1ST 2ND MO" TO VDUBUF
           ELSE
           IF SP-BANKRULE = "B"
              MOVE "30 AFTER 1 MO" TO VDUBUF
           ELSE
           IF SP-BANKRULE = "C"
              MOVE "30/1ST MO FOL" TO VDUBUF
           ELSE
           IF SP-BANKRULE = "D"
              MOVE "WRLD/1ST FOL" TO VDUBUF
           ELSE
           IF SP-BANKRULE = "E"
              MOVE "30 AFTER 1 MO" TO VDUBUF
           ELSE
           IF SP-BANKRULE = "F"
              MOVE "WRLD/SEMI-MONTHLY" TO VDUBUF.
           PERFORM SCREENIO.
      *---------- A50 VERSION ----------------------
       VALIDATE-RECFRMLA.
           MOVE "S  A090120D."  TO VDU.
           IF SP-RECFRMLA = " "
              MOVE "TRUE DAYS"  TO VDUBUF
           ELSE
           IF SP-RECFRMLA = "C"
              MOVE "CALENDAR"   TO VDUBUF
           ELSE
           IF SP-RECFRMLA = "D"
              MOVE "CAL DUEDAY"  TO VDUBUF
           ELSE
           IF SP-RECFRMLA = "P"
              MOVE "% OF PAYMT" TO VDUBUF.
           PERFORM SCREENIO.

       VALIDATE-RECPAYCD.
           MOVE "S  A010121D." TO VDU.
           MOVE "INVALID" TO VDUBUF.
           IF SP-RECPAYCD = " "
              MOVE "PRINC ONLY   " TO VDUBUF.
           IF SP-RECPAYCD = "A"
              MOVE "TOTAL MONIES " TO VDUBUF.
           IF SP-RECFRMLA NOT = "P"
              MOVE " " TO VDUBUF.
           PERFORM SCREENIO.

       VALIDATE-RECPDTH.
           MOVE "S  A010122D." TO VDU.
           MOVE " " TO VDUBUF.
           IF SP-RECPDTH NOT = 0
              MOVE "%" TO VDUBUF.
           IF SP-RECFRMLA NOT = "P"
              MOVE " " TO VDUBUF.
           PERFORM SCREENIO.
      *--------------------------------------------
       VALIDATE-CONTRACTUAL.
           MOVE "S  A130051." TO VDU.
           MOVE "INVALID" TO VDUBUF.
           IF SP-CONTRFRMLA = " "
              MOVE "PRINC ONLY   " TO VDUBUF.
           IF SP-CONTRFRMLA = "A"
              MOVE "TOTAL MONIES " TO VDUBUF.
           PERFORM SCREENIO.
       VALIDATE-1098FRMLA.
           MOVE "S  A100031." TO VDU.
           MOVE "INVALID" TO VDUBUF.
           IF SP-1098FRMLA = " "
              MOVE "INTEREST" TO VDUBUF.
           IF SP-1098FRMLA = "A"
              MOVE "PRO RATA" TO VDUBUF.
           IF SP-1098FRMLA = "B"
              MOVE "UNEARNED" TO VDUBUF.
           IF SP-1098FRMLA = "C"
              MOVE "ACTUARIAL" TO VDUBUF.
           IF SP-1098FRMLA = "D"
              MOVE "RULE 78THS" TO VDUBUF.
           PERFORM SCREENIO.
       VALIDATE-DISFRMLA.
           MOVE "S  A100042." TO VDU.
           MOVE "INVALID" TO VDUBUF.
           IF SP-DISFRMLA = "A"
              MOVE "ACTUARIAL" TO VDUBUF.
           IF SP-DISFRMLA = "B"
              MOVE "ACT/NO EXT" TO VDUBUF.
           IF SP-DISFRMLA = "C"
              MOVE "U.S. RULE" TO VDUBUF.
           PERFORM SCREENIO.

       SEND-LCTYPE-DESC.
           MOVE "S  A070171." TO VDU.
           MOVE "INVALID" TO VDUBUF.
           IF SP-LCTYPE = " "
              MOVE "N/A" TO VDUBUF
           ELSE IF SP-LCTYPE = "A"
              MOVE "% REGPM" TO VDUBUF
           ELSE IF SP-LCTYPE = "B"
              MOVE "$ AMT" TO VDUBUF
           ELSE IF SP-LCTYPE = "C"
              MOVE "% OF LATE" TO VDUBUF
           ELSE IF SP-LCTYPE = "D"
              MOVE "% PER MO" TO VDUBUF
           ELSE IF SP-LCTYPE = "E"
              MOVE "% RGPM TRUNC" TO VDUBUF
           ELSE IF SP-LCTYPE = "F"
              MOVE "% RGPM RNDUP" TO VDUBUF
           ELSE IF SP-LCTYPE = "G"
              MOVE "% PMT" TO VDUBUF
           ELSE IF SP-LCTYPE = "H"
              MOVE "% PMT TRUNC" TO VDUBUF
           ELSE IF SP-LCTYPE = "I"
              MOVE "% PMT RNDUP" TO VDUBUF
           ELSE IF SP-LCTYPE = "J"
              MOVE "% OF WLATE" TO VDUBUF
           ELSE IF SP-LCTYPE = "K"
              MOVE "% OF RLATE" TO VDUBUF
           ELSE IF SP-LCTYPE = "L"
              MOVE "% OF WRLATE" TO VDUBUF
           ELSE IF SP-LCTYPE = "M"
              MOVE "% LATE - MF" TO VDUBUF
           ELSE IF SP-LCTYPE = "N"
              MOVE "% WHOLE-MF" TO VDUBUF
           ELSE IF SP-LCTYPE = "O"
              MOVE "% DELINQ-MF" TO VDUBUF
           ELSE IF SP-LCTYPE = "P"
              MOVE "OH,P071296" TO VDUBUF
           ELSE IF SP-LCTYPE = "Q"
              MOVE "FLAT CHARGE" TO VDUBUF.
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
       NOT-EXPIRED.
           MOVE "THIS RECORD HAS NOT EXPIRED - CANNOT DELETE" TO MESS.
           PERFORM SEND-MESS.
       DISPLAY-SUBCLASS-DESC.
           MOVE "S  A150DESC. " TO VDU.
           MOVE SP-DESC TO VDUBUF.
           PERFORM SCREENIO.

      *********************************************
       IO-ROUTINES SECTION.
      *********************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-ST1-FILE.
           PERFORM CLOSE-SP1-FILE.
           PERFORM CLOSE-LC1-FILE.
           PERFORM CLOSE-BR-FILE.
       COPY "LIBLP/LPSTGS_SQL.CPY".
       COPY "LIBLP/LPSPGS_SQL.CPY".
       COPY "LIBLP/LPLCGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBLP/LPSP1IN.CPY".
       COPY "LIBLP/LPST1RN.CPY".
       COPY "LIBLP/LPLC1RN.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO ERRCD.

      *********************************************
       END-ROUTINE SECTION.
      *********************************************
           PERFORM CLOSE-FILES.
       EXIT-PROG.
       STOP-RUN.
           PERFORM FORM-RESET
           DESTROY SPMAN1-SCREEN.
           MOVE FORM-PATH TO FORM-PATHNAME.
           MOVE SPMAIN-LINK TO FORM-LINK-AREA.
           EXIT PROGRAM.
