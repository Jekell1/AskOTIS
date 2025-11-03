       IDENTIFICATION DIVISION.
       PROGRAM-ID.      SPMANI.
      ***************************************************************
      *   DIR :  MN
      *   DESC:  STATE PARAMETER MAINT
      *   PAGE 18
      *
      * REV:
      *  MJD 050895 ADDED CODE TO WRITE CHANGE FILE AFTER REWRITE
      *  SLC 051895 ADDED SP-CONTR-CREDIT
      *  JTG 062695 ADDED SP-CONTR-CREDIT-MIN-DEL-PER
      *  JTG 062995 ADDED SP-OPT-CAL-CHGABLE, MERCURY MI SERCHG
      *  JTG 071995 ADDED SP-PBDEL-DAYS
      *  JTG 072195 ADDED SP-PBINCR-PERCENT
      *                   SP-CPOT30-CD    SP-RDPOT30-CD
      *                   SP-CPOTCHGOFF-CD   SP-RDPOTCHGOFFCD
      *  JFF 120595 ADDED LOAN CLASS SCAN & STATE PARAMETER SCAN
      *  JTG 120496 CHANGED THE FOLLOWING TO ALLOW A 'Z' AND NOT 'B'
      *             TO MATCH LOGIC IN LONPFA.C & LONPFB.C
      *             88  SP-DLRVCHGS-RB-CD-VALID        VALUE " " "A" "Z".
      *             88  SP-PARVHELD-RB-CD-VALID        VALUE " " "A" "Z".
      *             88  SP-PARVPAID-RB-CD-VALID        VALUE " " "A" "Z".
      *  JTG 041097 REMOVED REFERENCE TO SP-EXPR FIELDS
      *  GLF 050897 FIXED EXPIRATION DATE (DID NOT DISPLAY, NOW IT DOES)
      *  BAH 102997 ADDED FEEFRMLA 'C' FOR DOC STAMPS, COMMERCIAL PR#579
      *  BLV 061198 CHANGE ORIG, CHANGED RECS TO USE SP-SIZE(4513) NOT 4502
      *  JTG 990309 ADDED SP-CPOTCHGOFF-CD-VALID 'B', REGENCY
      *  JTG 991203 ADDED SP-ALLOW-LN-BACK-DATE, REGMGM# 1029
      *  JTG 000125 ADDED SP-CAL-MAX-AT-ESTIMATE INSTANT AUTO #1047
      *            AND ADDED SP-ALLOW-LN-BACK-DATE, I-AM-WISC
      *  BAH 000503 ADDED GP-I-AM-NEWSOUTH TO SP-ALLOW-LN-BACK-DATE, PR# 1179
      *  BLV 000508 ALLOW SP-CPOTCHGOFF-CD OF 'C', MERC PR# 230,
      *             CONTRACTUAL POTENTIAL CHARGE-OFFS AS 120+
      *  MJD 000928 ADDED F2-MEMO OPTION FOR NEW SPR MEMO FILE (SMFILE)
      *  BAH 010816 ALLOW GP-I-AM-REGENCY TO SP-ALLOW-LN-BACK-DATE, PR# 1558
      *  BAH 040322 ADDED GP-I-AM-PACESETTER TO SP-ALLOW-LN-BACK-DATE
      *  BAH 041213 ADDED (GLOBAL) COLTN TO SP-ALLOW-LN-BACK-DATE, PR# 2434
      *  BAH 051018 CHANGED CHECK ON SP-ALLOW-LN-BACK-DATE, YOU DONT
      *             HAVE TO ADD SPECIFIC CUSTOMER NAMES ANYMORE, IT
      *             WILL ASK FOR DAILY PASSWORD TO TURN ON THIS FLAG
      *             SUPPORT, JUDY,LORI, PR# 2619
      *  MJD 090406 ADDED FIELD 19 FOR SP-ALLOW-ZERO-PROC REGMGM #3341
      *  MJD 111201 ADDED FIELD 20 FOR SP-ADV-TO-1ST-NO-CORR  PR#3907
      *   CS 120717 ADD #21 SP-LC-INT-ON-REPO-NON-PL  REGACC PR#314
      *             IF ACCT IS 'SX' OR 'SS' DONT ASSESS OR COLLECT LATE CHARGE
      *             OR INTEREST.
      *   CS 121005 ADD SP-CONTRACTUAL-SET-NONACCRUAL, SP-CONTRACUTAL-RESET-
      *             ACCRUAL, SP-MONTHS-PAY-NAPP-COUNTER REGACC PR#323
      *  BAH 130501 ADDED SP-SHORT-PAY-THRESHOLD FOR NEWPORT PR# 4256
      *  BAH 131130 ADDED SP-CASH-AVAIL-PERCENT FOR WORLD PR# 944
      *  BAH 151103 ADDED 27. SP-NET-BAL-REQ AND 28. SP-AMT-PROCEEDS-REQ
      *                                                  FOR REGMGM PR# 4885
      *   CS 160304 ADDED 29, SP-ALT-PREPAY-MONTHS PR#1140 WORLD
      *
      * KEC 2016.0316 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         CHANGED CD-FEE???? TO CD-GC-????.
      *  CS 160803 CHANGED SAVEKEY FROM X(8) TO X(9), ORIGINAL-SP-REC TO
      *            ORIGINAL-SP-REC, CHANGED-SP-REC TO CHANGED-SP-REC
      *  CS 170505 ACTIVATE 8 DIGIT DATES
      ***************************************************************
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
           "@(#)MN/SPMANI.CBL S35 04/11/22-12:10:57 >".
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
       COPY "LIBLP/LP01CDB.CPY".
       COPY "LIBLP/LP01CDB_SQL.CPY".
       COPY "LIBLP/LPWSCDB.CPY".
      *    **********************************
      *    *      PROGRAM WORKERS
      *    **********************************

       01  ONE               PIC S9.
       01  ELE               PIC 99     VALUE 0.
       COPY "LIBLP/SPMANSEL.CPY".
       01  IO-TYPE           PIC X(3)   VALUE SPACES.
       01  NEW-FLAG          PIC 9      VALUE 0.
       01  FIRST-SCAN        PIC 9      VALUE 0.
       01  TEST-CHAN         PIC X(6).
       01  TEST-CHANX REDEFINES TEST-CHAN.
           03  FILLER        PIC XX.
           03  GOOD-CHAN    PIC X(4).

       01  ORIGINAL-SP-REC      PIC X(SP-SIZE) VALUE SPACES.
       01  CHANGED-SP-REC       PIC X(SP-SIZE) VALUE SPACES.

       01  SPMAIN-LINK VALUE SPACES.
           03  FORM-NAME     PIC X(20).
           03  SAVEKEY       PIC X(9).
           03  NEW-REC       PIC X.
           03  ERRCD         PIC X.
           03  FORMSEL       PIC 99.
           03  SPMAIN-WINDOW-HANDLE PIC X(10).


      *    **********************************
      *    *      SCREEN DISPLAY BUFFER1
      *    **********************************
       01  DISPLAY-BUF      VALUE SPACES.
           03  A01           PIC X.
           03  A02           PIC X.
           03  A03           PIC X.
           03  A04           PIC X.
           03  A05           PIC X.
           03  A06           PIC X.
           03  A07           PIC X.
           03  A071          PIC Z9.99.
           03  A072          PIC ZZ9.9.
           03  A073          PIC X.
           03  A08           PIC X.
           03  A09           PIC ZZ9.
           03  A10           PIC ZZ9.
           03  A10-PER       PIC X.
           03  A11           PIC X.
           03  A12           PIC X.
           03  A13           PIC X.
           03  A14           PIC X.
           03  A15           PIC X.
           03  A151          PIC ZZ.
           03  A152          PIC X(7).
           03  A16           PIC X.
           03  A161          PIC ZZ.
           03  A162          PIC X(7).
           03  A17           PIC X.
           03  A18           PIC X.
           03  A19           PIC X.
           03  A20           PIC X.
           03  A21           PIC X.
           03  A22           PIC ZZ9.
           03  A23           PIC ZZ9.
           03  A24           PIC Z9.
           03  A25           PIC ZZ9.
           03  A26           PIC Z9.
           03  A27           PIC ZZ9.
           03  A28           PIC Z(6)9.
           03  A29           PIC Z9.
       01  DISPLAY-LIST.
           03  FILLER        PIC X(48)  VALUE
           "010 020 030 040 050 060 070 071 072 073 080 090".
           03  FILLER        PIC X(24)  VALUE
           "100 101 110 120 130 140".
           03  FILLER        PIC X(44)  VALUE
           "150 151 152 160 161 162 170 180 190 200 210".
           03  FILLER        PIC X(32)  VALUE
           "220 230 240 250 260 270 280 290.".

      *    **********************************
      *    *   ELEMENT SPECIFICATIONS
      *    **********************************
       01  MAX               PIC 99 COMP-3 VALUE 29.
       01  SPEC-TABLE.
      * 1. SP-IRREGULAR-PAY-FG
           03  FILLER        PIC X(8)   VALUE "E  A010 ".
      * 2. SP-DLRVCHGS-RB-CD(1)
           03  FILLER        PIC X(8)   VALUE "E  A010 ".
      * 3. SP-DLRVCHGS-RB-CD(2)
           03  FILLER        PIC X(8)   VALUE "E  A010 ".
      * 4. SP-DLRVCHGS-RB-CD(3)
           03  FILLER        PIC X(8)   VALUE "E  A010 ".
      * 5. SP-PARVHELD-RB-CD
           03  FILLER        PIC X(8)   VALUE "E  A010 ".
      * 6. SP-PARVPAID-RB-CD
           03  FILLER        PIC X(8)   VALUE "E  A010 ".
      * 7. SP-CONTR-CREDIT-CD
           03  FILLER        PIC X(8)   VALUE "E  A010 ".
      * 8. SP-OPT-CAL-CHGABLE
           03  FILLER        PIC X(8)   VALUE "E  A010 ".
      * 9. SP-PBDEL-DAYS
           03  FILLER        PIC X(8)   VALUE "ENNN030 ".
      *10. SP-PBINCR-PERCENT
           03  FILLER        PIC X(8)   VALUE "ENNN030 ".
      *11. SP-CPOT30-CD
           03  FILLER        PIC X(8)   VALUE "E  A010 ".
      *12. SP-CPOTCHGOFF-CD
           03  FILLER        PIC X(8)   VALUE "E  A010 ".
      *13. SP-RDPOT30-CD
           03  FILLER        PIC X(8)   VALUE "E  A010 ".
      *14. SP-RDPOTCHGOFF-CD
           03  FILLER        PIC X(8)   VALUE "E  A010 ".
      *15. SP-FEEFRMLA(1)
           03  FILLER        PIC X(8)   VALUE "E  A010 ".
      *16. SP-FEEFRMLA(2)
           03  FILLER        PIC X(8)   VALUE "E  A010 ".
      *17. SP-ALLOW-LN-BACK-DATE
           03  FILLER        PIC X(8)   VALUE "E  A010 ".
      *18. SP-CAL-MAX-IB-AT-ESTIMATE
           03  FILLER        PIC X(8)   VALUE "E  A010 ".
      *19. SP-ALLOW-ZERO-PROC
           03  FILLER        PIC X(8)   VALUE "E  A010 ".
      *20. SP-ADV-TO-1ST-NO-CORR
           03  FILLER        PIC X(8)   VALUE "E  A010 ".
      *21. SP-LC-INT-ON-REPO-NON-PL.
           03  FILLER        PIC X(8)   VALUE "E  A010 ".
      *22. SP-CONTRACTUAL-SET-NONACCRUAL
           03  FILLER        PIC X(8)   VALUE "ENNN030 ".
      *23. SP-CONTRACTUAL-RESET-ACCRUAL
           03  FILLER        PIC X(8)   VALUE "ENNN030 ".
      *24. SP-MONTHS-PAY-NAPP-COUNTER
           03  FILLER        PIC X(8)   VALUE "ENNN020 ".
      *25. SP-SHORT-PAY-THRESHOLD
           03  FILLER        PIC X(8)   VALUE "ENNN030 ".
      *26. SP-CASH-AVAIL-PERCENT
           03  FILLER        PIC X(8)   VALUE "ENNN020 ".
      *27. SP-NET-BAL-REQ
           03  FILLER        PIC X(8)   VALUE "ENNN030 ".
      *28. SP-AMT-PROCEEDS-REQ
           03  FILLER        PIC X(8)   VALUE "ENNN070 ".
      *29. SP-ALT-PREPAY-MONTHS         
           03  FILLER         PIC X(8)  VALUE "ENNN020 ".

       01  SPEC-TAB       REDEFINES SPEC-TABLE.
           03  SPEC-REC   OCCURS 29 TIMES.
               05  SPEC   PIC X(7).
               05  CHFG   PIC X.

       COPY "LIBWI/SPSCANW.CPY".
       COPY "LIBCL/CLMENUW_EXT.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBWI/CDBSCNW.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBWI/LCSCANW.CPY".
       COPY "LIBWI/MEMOSPW.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBMN/SPMANI_DEF.CPY".
       COPY "LIBMN/SPMANI_WKS.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/ACCESSW.CPY".

       COPY "LIBGB/GETFMW.CPY".
       01 FORM-LINK-AREA    PIC X(2000).

       SCREEN SECTION.
       COPY "LIBMN/SPMANI_SCN.CPY".

       PROCEDURE DIVISION
            USING FORM-PATHNAME FORM-LINK-AREA EXIT-PATHNAME.

      **********************************************
       MAIN-PROGRAM SECTION.
      **********************************************
           PERFORM SQL-CONNECT.

       COPY "LIBGB/CHKSEC.CPY".

           MOVE " " TO VDU-WIMENU-SELECTION
                       VDU-PGMENU-SELECTION.

           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "SPMANI" TO VDU-FORM-NAME.
           MOVE FORM-LINK-AREA TO SPMAIN-LINK.

       MAIN-MODULE.
      D    STOP MESS.
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
              MOVE "P" TO FORM-P2
              MOVE 19 TO FORM-SEL2.

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

      **********************************************
       REC-SP-MODULE SECTION.
      **********************************************
       REC-SP-MODULE-NO.
           MOVE SPACES TO IO-TYPE.
           IF SAVEKEY NOT = " "
              MOVE SAVEKEY TO SP1-KEY
              MOVE " " TO SAVEKEY
              PERFORM DISPLAY-SCAN-KEY
              GO TO READ-SPFILE.
           MOVE 0 TO NEW-FLAG.
           MOVE "SPCLASS SUBCLASS LCDESC CLASS STDESC SPNO." TO WR-LIST.
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
                 MOVE "SNNN020SPCLASS." TO VDU
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

      **********************************************
       CLASS-SCAN SECTION.
      **********************************************
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

      **********************************************
       ENTER-SCANKEY SECTION.
      **********************************************
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

      **********************************************YY
       CODE-SCAN SECTION.
      **********************************************YY
           MOVE "GC"                TO CDBSCN-TYPE.
           MOVE EXT-CONAME-USER-LOC TO CDBSCN-BRANCH.
           MOVE SPACES              TO CDBSCN-CODE.
           MOVE 1                   TO CDBSCN-BEGIN-X.
           MOVE "WI/CDBSCN" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH CDBSCN-BUF EXIT-PATHNAME.
           CANCEL FORM-PROGX.
           IF ( CDBSCN-STATUS NOT = "00") OR (CDBSCN-CODE = SPACES) 
              MOVE 9 TO IO-FG
              GO TO EXIT-CODE-SCAN.

           MOVE "S  A020091." TO VDU.
           MOVE ELE TO VDUROW.
           MOVE CDBSCN-CODE TO VDUBUF.
           PERFORM SCREENIO.

           MOVE CDBSCN-CODE TO VDUBUF.
           MOVE 0 TO IO-FG.

       EXIT-CODE-SCAN.
           EXIT.

      *************************************************
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
           IF ACTION = SPACES
              IF IO-FG NOT = 0
                 GO TO MODIFY-ROUTINE
              ELSE
                 MOVE "RWR" TO IO-TYPE
                 GO TO END-ENTRY-MODULE.
      * FORM-NUM1 CHECKS FOR PAGES 1 THRU 9
           IF FORM-KEY1 = " " AND FORM-P1 = "P" AND FORM-SEL1 NUMERIC
              IF FORM-SEL1 < 1 OR FORM-SEL1 > 17
                 GO TO MODIFY-ROUTINE
              ELSE
                 GO TO CHECK-PAGE.
      * FORM-NUM2 CHECKS FOR PAGE 10
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
                 A-26 A-27 A-28 A-29
                 DEPENDING ON ELE.
           GO TO END-ENTRY-MODULE.

      *    **********************************
      *    *      SET RECORD ELEMENTS
      *    **********************************
       A-01.
           MOVE VDUBUF TO SP-IRREGULAR-PAY-FG.
           IF NOT (SP-IRREGULAR-PAY-FG = " " OR "L")
              MOVE "ENTER: SPACE-FIRST, L-LAST" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       A-02.
           MOVE VDUBUF TO SP-DLRVCHGS-RB-CD(1).
           IF NOT SP-DLRVCHGS-RB-CD-VALID(1)
              MOVE "ENTER: ' ' OR 'A' OR 'Z'" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           IF LC-SALEFIN = "N"
              IF SP-DLRVCHGS-RB-CD(1) NOT = " "
                 PERFORM NO-DIRECT-LOANS
                 GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       A-03.
           MOVE VDUBUF TO SP-DLRVCHGS-RB-CD(2).
           IF NOT SP-DLRVCHGS-RB-CD-VALID(2)
              MOVE "ENTER: ' ' OR 'A' OR 'Z'" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           IF LC-SALEFIN = "N"
              IF SP-DLRVCHGS-RB-CD(2) NOT = " "
                 PERFORM NO-DIRECT-LOANS
                 GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       A-04.
           MOVE VDUBUF TO SP-DLRVCHGS-RB-CD(3).
           IF NOT SP-DLRVCHGS-RB-CD-VALID(3)
              MOVE "ENTER: ' ' OR 'A' OR 'Z'" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           IF LC-SALEFIN = "N"
              IF SP-DLRVCHGS-RB-CD(3) NOT = " "
                 PERFORM NO-DIRECT-LOANS
                 GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       A-05.
           MOVE VDUBUF TO SP-PARVHELD-RB-CD.
           IF NOT SP-PARVHELD-RB-CD-VALID
              MOVE "ENTER: ' ' OR 'A' OR 'Z'" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           IF LC-SALEFIN = "N"
              IF SP-PARVHELD-RB-CD NOT = " "
                 PERFORM NO-DIRECT-LOANS
                 GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       A-06.
           MOVE VDUBUF TO SP-PARVPAID-RB-CD.
           IF NOT SP-PARVPAID-RB-CD-VALID
              MOVE "ENTER: ' ' OR 'A' OR 'Z'" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           IF LC-SALEFIN = "N"
              IF SP-PARVPAID-RB-CD NOT = " "
                 PERFORM NO-DIRECT-LOANS
                 GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       A-07.
           MOVE VDUBUF TO SP-CONTR-CREDIT-CD.
           IF NOT SP-CONTR-CREDIT-CD-VALID
              MOVE "ENTER ' ' OR 'A' OR 'B' ONLY!" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           IF SP-CONTR-CREDIT-CD = " "
              MOVE "S  A050071." TO VDU
              PERFORM SCREENIO
              MOVE 0 TO SP-CONTR-CREDIT
              MOVE "S  A050072." TO VDU
              PERFORM SCREENIO
              MOVE 0 TO SP-CONTR-CREDIT-MIN-DEL-PER
              MOVE "S  A010073." TO VDU
              PERFORM SCREENIO
              GO TO CREATE-ROUTINE.
       A-071.
           MOVE "ENNN022071." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO A-071.
           MOVE VDUNUM2 TO SP-CONTR-CREDIT.
       A-072.
           MOVE "ENNN031072." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO A-071.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO A-072.
           MOVE VDUNUM1 TO SP-CONTR-CREDIT-MIN-DEL-PER.
           MOVE "S  A010073." TO VDU.
           MOVE "%" TO VDUBUF.
           PERFORM SCREENIO.
           GO TO CREATE-ROUTINE.
       A-08.
           MOVE VDUBUF TO SP-OPT-CAL-CHGABLE.
           IF NOT SP-OPT-CAL-CHGABLE-VALID
              MOVE "ENTER: ' ', 'P', 'I', 'S', OR 'B'" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       A-09.
           MOVE VDUNUM0 TO SP-PBDEL-DAYS.
           GO TO CREATE-ROUTINE.
       A-10.
           MOVE VDUNUM0 TO SP-PBINCR-PERCENT.
           MOVE "S  A010101." TO VDU.
           MOVE "%" TO VDUBUF.
           PERFORM SCREENIO.
           GO TO CREATE-ROUTINE.
       A-11.
           MOVE VDUBUF TO SP-CPOT30-CD.
           IF NOT SP-CPOT30-CD-VALID
              MOVE "ENTER ' ' OR 'A' ONLY" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       A-12.
           MOVE VDUBUF TO SP-CPOTCHGOFF-CD.
           IF NOT SP-CPOTCHGOFF-CD-VALID
              MOVE "ENTER ' ', 'A', 'B', OR 'C' ONLY" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       A-13.
           MOVE VDUBUF TO SP-RDPOT30-CD.
           IF NOT SP-RDPOT30-CD-VALID
              MOVE "ENTER ' ' ONLY" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       A-14.
           MOVE VDUBUF TO SP-RDPOTCHGOFF-CD.
           IF NOT SP-RDPOTCHGOFF-CD-VALID
              MOVE "ENTER ' ' OR 'A' ONLY" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       A-15.
           MOVE VDUBUF TO SP-FEEFRMLA(1).
           IF NOT SP-FEEFRMLA-VALID(1)
              MOVE "ENTER: SPACE, 'A', 'B' OR 'C' " TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           IF SP-FEEFRMLA(1) = " "
              PERFORM RESET-15
              PERFORM RESET-16
              MOVE"N" TO CHFG(16)
              GO TO CREATE-ROUTINE.
           MOVE " " TO CHFG(16).
       A-151.
           MOVE "^-BACK, F6-SCAN, ENTER FEE CODE #1:" TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "E  A020151." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.

           IF VDUSTATUS = "1<"
              PERFORM CODE-SCAN
              IF IO-FG NOT = 0
                 GO TO A-151.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO A-151.
           MOVE VDUBUF TO SP-FEECD(1).
           IF SP-FEECD(1) NOT = " "
              MOVE SP-FEECD(1) TO CDB-CODE
              PERFORM GET-CODE
              IF IO-FG = 9
                 MOVE "INVALID FEE CODE" TO MESS
                 PERFORM SEND-MESS
                 GO TO A-151.
           MOVE "S  A020152." TO VDU.
           MOVE CDB-DESC TO VDUBUF.
           PERFORM SCREENIO.
           MOVE " " TO MESS.
           PERFORM SEND-LEGEND.
       A-152.
           MOVE "^-BACK, ENTER FEE TITLE #1:" TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "E  A080152." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO A-152.
           MOVE VDUBUF TO SP-FEEID(1).
           IF SP-FEEID(1) = " "
              GO TO A-152.
           GO TO CREATE-ROUTINE.
       A-16.
           MOVE VDUBUF TO SP-FEEFRMLA(2).
           IF NOT SP-FEEFRMLA-VALID(2)
              MOVE "ENTER: SPACE, 'A', 'B' OR 'C' " TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           IF SP-FEEFRMLA(2) = " "
              PERFORM RESET-16
              GO TO CREATE-ROUTINE.
       A-161.
           MOVE "^-BACK, F6-SCAN, ENTER FEE CODE #2:" TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "E  A020161." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.

           IF VDUSTATUS = "1<"
              PERFORM CODE-SCAN
              IF IO-FG NOT = 0
                 GO TO A-161.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO A-161.
           MOVE VDUBUF TO SP-FEECD(2).
           IF SP-FEECD(2) NOT = " "
              MOVE SP-FEECD(2) TO CDB-CODE
              PERFORM GET-CODE
              IF IO-FG = 9
                 MOVE "INVALID FEE CODE" TO MESS
                 PERFORM SEND-MESS
                 GO TO A-161.
           MOVE "S  A020162." TO VDU.
           MOVE CDB-DESC TO VDUBUF.
           PERFORM SCREENIO.
           MOVE " " TO MESS.
           PERFORM SEND-LEGEND.
       A-162.
           MOVE "^-BACK, ENTER FEE TITLE #1:" TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "E  A080162." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO A-162.
           MOVE VDUBUF TO SP-FEEID(2).
           IF SP-FEEID(2) = " "
              GO TO A-162.
           GO TO CREATE-ROUTINE.
       A-17.
           MOVE VDUBUF TO SP-ALLOW-LN-BACK-DATE.
      * ANYONE CAN ENTER "N"
           IF SP-ALLOW-LN-BACK-DATE = "N"
              GO TO CREATE-ROUTINE.

           IF NOT (SP-ALLOW-LN-BACK-DATE = "Y" OR "N")
              MOVE "ENTER 'Y' OR 'N' ONLY" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.

      * ONLY GET HERE IF THEY ENTERED "Y" AND NOT AUTHORIZED COMPANY
           MOVE "MUST ENTER DAILY PASSWORD FOR 'Y':" TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "E RA060SEL." TO VDU.
           PERFORM SCREENIO.
           MOVE VDUBUF TO TEST-CHAN.
           MOVE SPACES TO MESS
           PERFORM SEND-LEGEND
           IF GOOD-CHAN = EXT-CONAME-DAILY-PASSWORD
              MOVE "PASSWORD ACCEPTED" TO MESS
              PERFORM SEND-MESS
              GO TO CREATE-ROUTINE
           ELSE
              MOVE "N" TO SP-ALLOW-LN-BACK-DATE
              MOVE "INVALID PASSWORD, ENTER 'N' OR VALID PASSWORD" TO
                           MESS PERFORM SEND-MESS
              GO TO ENTER-ELE

           GO TO CREATE-ROUTINE.
       A-18.
           MOVE VDUBUF TO SP-CAL-MAX-AT-ESTIMATE.
           IF NOT (SP-CAL-MAX-AT-ESTIMATE = "Y" OR "N")
              MOVE "ENTER 'Y' OR 'N' ONLY" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       A-19.
           MOVE VDUBUF TO SP-ALLOW-ZERO-PROC.
           IF NOT (SP-ALLOW-ZERO-PROC = "Y" OR "N")
              MOVE "ENTER 'Y' OR 'N' ONLY" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       A-20.
           MOVE VDUBUF TO SP-ADV-TO-1ST-NO-CORR
           IF NOT (SP-ADV-TO-1ST-NO-CORR = "Y" OR "N" OR "A")
              MOVE "ENTER 'Y' OR 'N' OR 'A' ONLY" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.


       A-21.
           MOVE VDUBUF TO SP-LC-INT-ON-REPO-NON-PL.
           IF NOT (SP-LC-INT-ON-REPO-NON-PL = "Y" OR "N")
              MOVE "ENTER 'Y' OR 'N'" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.

           GO TO CREATE-ROUTINE.
       A-22.
           MOVE VDUNUM0 TO SP-CONTRACTUAL-SET-NONACCRUAL.
           GO TO CREATE-ROUTINE.

       A-23.
           MOVE VDUNUM0 TO SP-CONTRACTUAL-RESET-ACCRUAL.
           GO TO CREATE-ROUTINE.

       A-24.
           MOVE VDUNUM0 TO SP-MONTHS-PAY-NAPP-COUNTER.
           GO TO CREATE-ROUTINE.
       A-25.
           MOVE VDUNUM0 TO SP-SHORT-PAY-THRESHOLD.
           GO TO CREATE-ROUTINE.
       A-26.
           MOVE VDUNUM0 TO SP-CASH-AVAIL-PERCENT.
           GO TO CREATE-ROUTINE.
       A-27.
           MOVE VDUNUM0 TO SP-NET-BAL-REQ.
           GO TO CREATE-ROUTINE.
       A-28.
           MOVE VDUNUM0 TO SP-AMT-PROCEEDS-REQ.
           GO TO CREATE-ROUTINE.
       A-29.
           MOVE VDUNUM0 TO SP-ALT-PREPAY-MONTHS.    
           GO TO CREATE-ROUTINE.

       END-ENTRY-MODULE.
           MOVE 0 TO FORM-SEL1 FORM-SEL2.
       EXIT-ENTRY-MODULE.
           EXIT.

      ************************************************
       WRITE-MODULE SECTION.
      ************************************************
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

      ************************************************
       SPSCAN-SCAN SECTION.
      ************************************************
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

      ************************************************
       DISPLAY-FIELDS SECTION.
      ************************************************
           MOVE SPACES TO DISPLAY-BUF.
           MOVE "S  8000EFFDATE." TO VDU.
           MOVE SP-EFFDATE TO VDU-DATE-YYYYMMDD.
           PERFORM SCREENIO.
           MOVE "S  8000EXPDATE." TO VDU.
           MOVE SP-EXPDATE TO VDU-DATE-YYYYMMDD.
           PERFORM SCREENIO.
           PERFORM DISPLAY-SUBCLASS-DESC.

           MOVE SP-IRREGULAR-PAY-FG TO A01.
           MOVE SP-DLRVCHGS-RB-CD(1) TO A02.
           MOVE SP-DLRVCHGS-RB-CD(2) TO A03.
           MOVE SP-DLRVCHGS-RB-CD(3) TO A04.
           MOVE SP-PARVHELD-RB-CD TO A05.
           MOVE SP-PARVPAID-RB-CD TO A06.
           MOVE SP-CONTR-CREDIT-CD TO A07.
           IF SP-CONTR-CREDIT-CD NOT = " "
              MOVE SP-CONTR-CREDIT TO A071
              MOVE SP-CONTR-CREDIT-MIN-DEL-PER TO A072
              MOVE "%" TO A073.
           MOVE SP-OPT-CAL-CHGABLE TO A08.
           MOVE SP-PBDEL-DAYS TO A09.
           MOVE SP-PBINCR-PERCENT TO A10.
           MOVE "%" TO A10-PER.
           MOVE SP-CPOT30-CD TO A11.
           MOVE SP-CPOTCHGOFF-CD TO A12.
           MOVE SP-RDPOT30-CD TO A13.
           MOVE SP-RDPOTCHGOFF-CD TO A14.

           MOVE SP-FEEFRMLA(1) TO A15.
           MOVE SP-FEECD(1) TO A151.
           MOVE SP-FEEID(1) TO A152.
           MOVE SP-FEEFRMLA(2) TO A16.
           MOVE SP-FEECD(2) TO A161.
           MOVE SP-FEEID(2) TO A162.
           MOVE SP-ALLOW-LN-BACK-DATE TO A17.
           MOVE SP-CAL-MAX-AT-ESTIMATE TO A18.
           MOVE SP-ALLOW-ZERO-PROC TO A19.
           MOVE SP-ADV-TO-1ST-NO-CORR TO A20.
           MOVE SP-LC-INT-ON-REPO-NON-PL TO A21.
           MOVE SP-CONTRACTUAL-SET-NONACCRUAL TO A22.
           MOVE SP-CONTRACTUAL-RESET-ACCRUAL TO A23.
           MOVE SP-MONTHS-PAY-NAPP-COUNTER  TO A24.
           MOVE SP-SHORT-PAY-THRESHOLD TO A25.
           MOVE SP-CASH-AVAIL-PERCENT TO A26.
           IF SP-NET-BAL-REQ = 202
              MOVE 0 TO A27
           ELSE
              MOVE SP-NET-BAL-REQ TO A27.
           IF SP-AMT-PROCEEDS-REQ = 2020202
              MOVE 0 TO A28
           ELSE
              MOVE SP-AMT-PROCEEDS-REQ TO A28.

           MOVE SP-ALT-PREPAY-MONTHS TO A29.
           MOVE DISPLAY-BUF TO WR-BUF.
           MOVE DISPLAY-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
       DISPLAY-FIELDS-EXIT.
           EXIT.

      ************************************************
       FORM-RESET SECTION.
      ************************************************
           MOVE EXT-CONAME-CONAME TO SPMANI-CONAME.
           MOVE EXT-CONAME-SYSDATE TO SPMANI-SYSDATE.
           DISPLAY SPACES UPON SPMAIN-WINDOW-HANDLE
                               LINE 0 COL 0 ERASE SCREEN.
           DISPLAY SPMANI-SCREEN UPON SPMAIN-WINDOW-HANDLE.
           MOVE SPMANI-FORM-FIELDS TO WR-BUF.
           MOVE SPMANI-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      ************************************************
       SETUP-LINK-INFO SECTION.
      ************************************************
           MOVE SPMANI-HELP--FILE TO HELP-LINK-FILE.
           MOVE SPMANI-HELP--DIR TO HELP-LINK-DIR.

      ************************************************
       VDU-CONVERT-FIELD SECTION.
      ************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBMN/SPMANI_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

       COPY "LIBGB/DATER.CPY".
       COPY "LIBLP/LPTBNO.CPY".
       COPY "LIBGB/SCREEN.CPY".

      ************************************************
       MISC-PARAGRAPHS SECTION.
      ************************************************
       COPY "LIBGB/ACCESS.CPY".
       SEND-LEGEND.
           MOVE "S  A000MSEL." TO VDU.
           MOVE MESS TO VDUBUF.
           PERFORM SCREENIO.
       GET-CODE.
           MOVE "GC"                TO CDB-TYPE.
           MOVE EXT-CONAME-USER-LOC TO CDB-BRANCH.
      *--> SET BEFORE                  CDB-CODE.
           PERFORM OPEN-CDB1-FILE.
           PERFORM READ-CDB1-FILE.
           PERFORM CLOSE-CDB1-FILE.
       RESET-15.
           MOVE "S  A020151." TO VDU.
           PERFORM SCREENIO.
           MOVE " " TO SP-FEECD(1).
           MOVE "S  A020152." TO VDU.
           PERFORM SCREENIO.
           MOVE " " TO SP-FEEID(1).
       RESET-16.
           MOVE "S  A010160." TO VDU.
           PERFORM SCREENIO.
           MOVE " " TO SP-FEEFRMLA(2).
           MOVE "S  A020161." TO VDU.
           PERFORM SCREENIO.
           MOVE " " TO SP-FEECD(2).
           MOVE "S  A080162." TO VDU.
           PERFORM SCREENIO.
           MOVE " " TO SP-FEEID(2).
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
       GET-LC.
           PERFORM OPEN-LC1-FILE.
           PERFORM READ-LC1-FILE.
           PERFORM CLOSE-LC1-FILE.
           IF IO-FG NOT = 0
              MOVE "* NOT ON FILE *" TO LC-DESC.
       VERIFY-LOAN-CLASS.
           MOVE SP-SPRCLASS TO LC-CODE.
           PERFORM GET-LC.
           MOVE "S  A160LCDESC. " TO VDU.
           MOVE LC-DESC TO VDUBUF.
           PERFORM SCREENIO.
       DISPLAY-SUBCLASS-DESC.
           MOVE "S  A150DESC. " TO VDU.
           MOVE SP-DESC TO VDUBUF.
           PERFORM SCREENIO.
       NO-DIRECT-LOANS.
           MOVE "MUST BE A SPACE FOR DIRECT LOAN CLASSES" TO MESS.
           PERFORM SEND-MESS.
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

      ************************************************
       IO-ROUTINES SECTION.
      ************************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-ST1-FILE.
           PERFORM CLOSE-SP1-FILE.
           PERFORM CLOSE-LC1-FILE.
           PERFORM CLOSE-CDB1-FILE.
       COPY "LIBLP/LPSTGS_SQL.CPY".
       COPY "LIBLP/LPSPGS_SQL.CPY".
       COPY "LIBLP/LPLCGS_SQL.CPY".
       COPY "LIBLP/LPCDBGS_SQL.CPY".
       COPY "LIBLP/LPSP1IN.CPY".
       COPY "LIBLP/LPST1RN.CPY".
       COPY "LIBLP/LPLC1RN.CPY".
       COPY "LIBLP/LPCDB1RN.CPY".
       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO ERRCD.

      ************************************************
       END-ROUTINE SECTION.
      ************************************************
           PERFORM CLOSE-FILES.
       EXIT-PROG.
       STOP-RUN.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY SPMANI-SCREEN.
           MOVE FORM-PATH TO FORM-PATHNAME.
           MOVE SPMAIN-LINK TO FORM-LINK-AREA.
           EXIT PROGRAM.
