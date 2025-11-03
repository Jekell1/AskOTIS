       IDENTIFICATION DIVISION.
       PROGRAM-ID.      SPMANK.
      ***************************************************************
      *   DIR :  LP
      *   DESC:  STATE PARAMETER MAINT - PAGE 20.
      *
      * REV:
      *  060493 JTG ADDED SP-FEEFRMLA, SP-FEECD
      *  041394 JTG EXPANDED SP-FEEFRMLA, SP-FEECD (TABLE OF 2)
      *  082796 KEC ON CREATED A NEW RECORD, SKIP TO #17.
      *  022097 KEC ADD SP-MIN-CREDIT-SCORE
      *  041097 JTG MADE SP-EXPR-FRMLA 'B' VALID AND ADDED
      *             88  SP-EXPR-FRMLA-ORIG         VALID "B".
      *             88  SP-EXPR-FRMLA-USE-EXPIRED  VALID "A" "B".
      *  050897 GLF ADDED EXPIRATION DATE TO TOP OF PAGE
      *  071897 JTG ADDED SP-PACESETTER-FG RE: #135 GRAND ISLAND
      *  072897 JTG ADDED SP-WAGE-ASSIGN-DAYS MERCURY #E135
      *  081397 JTG CORRECTED BUG IN SPEC-TABLE ONE SPEC TO MANY
      *  051298 JTG ADDED SP-CAL-PAYMENT-ROUND-FG, CAVILER #XXX
      *  BLV 061198 CHANGE ORIG, CHANGED RECS TO USE SP-SIZE(4513) NOT 4502
      *  KEC 092398 ADDED LOGIC TO GO TO NEW PAGE #21. REGENCY REVOLVING
      *  BLV 991117 ADDED SP-RENEWABLE-PAYS, SP-RENEWABLE-DAYS: WORLD PR# 213
      *  BLV 000202 ADDED SIGNS TO SP-RENEWABLE-PAYS, SP-RENEWABLE-DAYS:
      *             WORLD PR# 213, ALLOW -1 FOR SPECIAL FORMULA
      *  MJD 000928 ADDED F2-MEMO OPTION FOR NEW SPR MEMO FILE (SMFILE)
      *  CS  040927 ADD CALL TO CL/SPMEMO WHICH WRITE A SPR CHANGE MEMO (TYPE A)
      *  BAH 050218 ADDED SP-EXPD-FRMLA "B" FOR LENDMARK PL# 463
      *   CS 050518 ADDED 'C' TO SP-EXPR-FRMLA(FIELD 21) JUDY PR#484
      *  BAH 071127 ADDED SP-MAX-CASH-PROCEEDS FOR REGMGM PR# 3110
      *
      *  CS 160803 CHANGED SAVEKEY FROM X(8) TO X(9), ORIGINAL-SP-REC TO
      *            ORIGINAL-SP-REC, CHANGED-SP-REC TO CHANGED-SP-REC
      *  CS 170505 ACTIVATE 8 DIGIT DATES
      ************************************************************************
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
           "@(#)MN/SPMANK.CBL S35 04/11/22-12:11:20 >".
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
       01  SUB               PIC 99     COMP-3 VALUE 0.
       01  ELE               PIC 99     VALUE 0.
       COPY "LIBLP/SPMANSEL.CPY".

       01  IO-TYPE           PIC X(3)   VALUE SPACES.
       01  NEW-FLAG          PIC 9      VALUE 0.
       01  FIRST-SCAN        PIC 9      VALUE 0.

       01  ORIGINAL-SP-REC      PIC X(SP-SIZE) VALUE SPACES.
       01  CHANGED-SP-REC       PIC X(SP-SIZE) VALUE SPACES.

       01  SPMAIN-LINK VALUE SPACES.
           03  FORM-NAME     PIC X(20).
           03  SAVEKEY       PIC X(9).
           03  NEW-REC       PIC X.
           03  ERRCD         PIC X.
           03  FORMSEL       PIC 99.
           03  SPMAIN-WINDOW-HANDLE PIC X(10).

      *01  FINCHG-DESC-TAB.
      *    03  FILLER        PIC X(11) VALUE "INT    ".
      *    03  FILLER        PIC X(11) VALUE "INT + EXT".
      *    03  FILLER        PIC X(11) VALUE "INT + SRV".
      *    03  FILLER        PIC X(11) VALUE "INT + FEE".
      *    03  FILLER        PIC X(11) VALUE "INT+EXT+SRV".
      *    03  FILLER        PIC X(11) VALUE "INT+EXT+FEE".
      *    03  FILLER        PIC X(11) VALUE "IT+EX+SR+FE".
      *    03  FILLER        PIC X(11) VALUE "INT+SRV+FEE".
      *    03  FILLER        PIC X(11) VALUE "SRV + FEE  ".
      *01  FINCHG-DESC-TABX REDEFINES FINCHG-DESC-TAB.
      *    03  FINCHG-DESC OCCURS 9 PIC X(11).
      *01  FINCHG-MAX   PIC 99 COMP-3 VALUE 8.


      *    **********************************
      *    *      SCREEN DISPLAY BUFFER1
      *    **********************************
       01  DISPLAY-BUF      VALUE SPACES.
           03  A01           PIC Z999.
           03  A02           PIC X.
           03  A03           PIC Z9.
           03  A04           PIC X.
           03  A05           PIC ZZZ9-.
           03  A06           PIC ZZZ9-.
           03  A07           PIC Z(5)9.99.
      *    03  A08
      *    03  A09
      *    03  A10
      *    03  A11
      *    03  A12
           03  A17           PIC X.
           03  A18           PIC ZZ.
           03  A19           PIC ZZ.
           03  A20           PIC X.
           03  A21           PIC X.
           03  A22           PIC X.
           03  A23           PIC X.
           03  A24           PIC ZZ9.999 BLANK ZERO.
           03  A241          PIC Z(5)9.99 BLANK ZERO.
           03  A242          PIC ZZZ.
           03  A243          PIC X.
           03  A25           PIC ZZ9.999 BLANK ZERO.
           03  A251          PIC Z(5)9.99 BLANK ZERO.
           03  A252          PIC ZZZ.
           03  A253          PIC X.
           03  A26           PIC ZZ9.999 BLANK ZERO.
           03  A261          PIC Z(5)9.99 BLANK ZERO.
           03  A262          PIC ZZZ.
           03  A263          PIC X.
           03  A27           PIC ZZ9.999 BLANK ZERO.
           03  A271          PIC Z(5)9.99 BLANK ZERO.
           03  A272          PIC ZZZ.
           03  A273          PIC X.
           03  A28           PIC ZZ9.999 BLANK ZERO.
           03  A281          PIC Z(5)9.99 BLANK ZERO.
           03  A282          PIC ZZZ.
           03  A283          PIC X.
       01  DISPLAY-LIST.
           03  FILLER        PIC X(48)  VALUE
           "010 020 030 040 050 060 070 170 180 190 200 210 ".
           03  FILLER        PIC X(56)  VALUE
           "220 230 240 241 242 243 250 251 252 253 260 261 262 263 ".
           03  FILLER        PIC X(32)  VALUE
           "270 271 272 273 280 281 282 283.".


      *    **********************************
      *    *   ELEMENT SPECIFICATIONS
      *    **********************************
       01  MAX               PIC 99  VALUE 28.
       01  SPEC-TABLE.
           03  FILLER        PIC X(8)   VALUE "EN N040 ".
           03  FILLER        PIC X(8)   VALUE "E  A010 ".
           03  FILLER        PIC X(8)   VALUE "ENNN020 ".
           03  FILLER        PIC X(8)   VALUE "E  A010 ".
      * 5. SP-RENEWABLE-PAYS
           03  FILLER        PIC X(8)   VALUE "EN S040 ".
      * 6. SP-RENEWABLE-DAYS
           03  FILLER        PIC X(8)   VALUE "EN S040 ".
      * 7. SP-MAX-CASH-PROCEEDS
           03  FILLER        PIC X(8)   VALUE "EN N062 ".
           03  FILLER        PIC X(8)   VALUE "E  A010N".
           03  FILLER        PIC X(8)   VALUE "E  A010 ".
      * 10
           03  FILLER        PIC X(8)   VALUE "E  A010 ".
           03  FILLER        PIC X(8)   VALUE "E  A010N".
           03  FILLER        PIC X(8)   VALUE "E  A010N".
           03  FILLER        PIC X(8)   VALUE "E  A010N".
           03  FILLER        PIC X(8)   VALUE "E  A010N".
      * 15
           03  FILLER        PIC X(8)   VALUE "E  A010N".
           03  FILLER        PIC X(8)   VALUE "E  A010N".
      * 17. SP-EXPD-FRMLA
           03  FILLER        PIC X(8)   VALUE "E  A010 ".
      * 18. SP-EXPD-MONTHS
           03  FILLER        PIC X(8)   VALUE "ENNN020 ".
      * 19. SP-EXPD-DAYS
           03  FILLER        PIC X(8)   VALUE "ENNN020 ".
      * 20. SP-EXPD-EXCEPT
           03  FILLER        PIC X(8)   VALUE "E  A010 ".
      * 21. SP-EXPR-FRMLA
           03  FILLER        PIC X(8)   VALUE "E  A010 ".
      * 22. SP-EXPR-LCHG
           03  FILLER        PIC X(8)   VALUE "E  A010 ".
      * 23. NA
           03  FILLER        PIC X(8)   VALUE "E  A010 ".
      * 24. SP-EXPR-RATE(1)
           03  FILLER        PIC X(8)   VALUE "ENNN033 ".
      * 25. SP-EXPR-RATE(2)
           03  FILLER        PIC X(8)   VALUE "ENNN033 ".
      * 26. SP-EXPR-RATE(3)
           03  FILLER        PIC X(8)   VALUE "ENNN033 ".
      * 27. SP-EXPR-RATE(2)
           03  FILLER        PIC X(8)   VALUE "ENNN033 ".
      * 28. SP-EXPR-RATE(3)
           03  FILLER        PIC X(8)   VALUE "ENNN033 ".
       01  SPEC-TAB       REDEFINES SPEC-TABLE.
           03  SPEC-REC   OCCURS 28 TIMES.
               05  SPEC   PIC X(7).
               05  CHFG   PIC X.

       COPY "LIBWI/SPSCANW.CPY".
       COPY "LIBCL/CLMENUW_EXT.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBWI/LCSCANW.CPY".
       COPY "LIBWI/MEMOSPW.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBMN/SPMANK_DEF.CPY".
       COPY "LIBMN/SPMANK_WKS.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/ACCESSW.CPY".

       COPY "LIBGB/GETFMW.CPY".
       01 FORM-LINK-AREA    PIC X(2000).

       SCREEN SECTION.
       COPY "LIBMN/SPMANK_SCN.CPY".

       PROCEDURE DIVISION
            USING FORM-PATHNAME FORM-LINK-AREA EXIT-PATHNAME.

      ******************************************
       MAIN-PROGRAM SECTION.
      ******************************************
           PERFORM SQL-CONNECT.

       COPY "LIBGB/CHKSEC.CPY".
           MOVE " " TO VDU-WIMENU-SELECTION
                       VDU-PGMENU-SELECTION.
           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "SPMANK" TO VDU-FORM-NAME.
           MOVE FORM-LINK-AREA TO SPMAIN-LINK.
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
              MOVE "P" TO FORM-P2
              MOVE 21 TO FORM-SEL2.
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

           IF ( ELE > 0 ) AND ( ELE < 17 )
              GO TO CREATE-ROUTINE.

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
              IF FORM-SEL1 < 1 OR FORM-SEL1 = 20
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
                 A-26 A-27 A-28
                 DEPENDING ON ELE.
           GO TO END-ENTRY-MODULE.

      *    **********************************
      *    *      SET RECORD ELEMENTS
      *    **********************************
       A-01.
           MOVE VDUNUM0 TO SP-MIN-CREDIT-SCORE.
           GO TO CREATE-ROUTINE.
       A-02.
           MOVE VDUBUF TO SP-PACESETTER-FG.
           IF NOT (SP-PACESETTER-FG = "Y" OR "N")
              MOVE "ENTER Y OR N" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       A-03.
           MOVE VDUNUM0 TO SP-WAGE-ASSIGN-DAYS.
           GO TO CREATE-ROUTINE.
       A-04.
           MOVE VDUBUF TO SP-CAL-PAYMENT-ROUND-FG.
           IF NOT (SP-CAL-PAYMENT-ROUND-FG = "S" OR "R" OR "N" OR "O")
              MOVE "ENTER S, R, N OR O  ONLY" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       A-05.
           MOVE VDUNUM0 TO SP-RENEWABLE-PAYS.
           IF SP-RENEWABLE-PAYS < -1
              MOVE "ONLY VALID FORMULA IS -1" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       A-06.
           MOVE VDUNUM0 TO SP-RENEWABLE-DAYS.
           IF SP-RENEWABLE-DAYS < -1
              MOVE "ONLY VALID FORMULA IS -1" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       A-07.
           MOVE VDUNUM2 TO SP-MAX-CASH-PROCEEDS.
           IF SP-MAX-CASH-PROCEEDS = 0
              MOVE "YOU HAVE SET 'NO CASH PROCEEDS ALLOWED'" TO MESS
              PERFORM SEND-MESS.
           GO TO CREATE-ROUTINE.
       A-08.
           GO TO CREATE-ROUTINE.
       A-09.
           GO TO CREATE-ROUTINE.
       A-091.
           GO TO CREATE-ROUTINE.
       A-092.
           GO TO CREATE-ROUTINE.
       A-10.
           GO TO CREATE-ROUTINE.
       A-101.
           GO TO CREATE-ROUTINE.
       A-102.
           GO TO CREATE-ROUTINE.
       A-11.
           GO TO CREATE-ROUTINE.
       A-12.
           GO TO CREATE-ROUTINE.
       A-13.
           GO TO CREATE-ROUTINE.
       A-14.
           GO TO CREATE-ROUTINE.
       A-15.
           GO TO CREATE-ROUTINE.
       A-16.
           GO TO CREATE-ROUTINE.
       A-17.
           MOVE VDUBUF TO SP-EXPD-FRMLA.
           IF NOT SP-EXPD-FRMLA-VALID
              MOVE "ENTER: SPACE, 'A' OR 'B'" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           PERFORM VALIDATE-EXPD-FRMLA.
           GO TO CREATE-ROUTINE.
       A-18.
           MOVE VDUNUM0 TO SP-EXPD-MONTHS.
           GO TO CREATE-ROUTINE.
       A-19.
           MOVE VDUNUM0 TO SP-EXPD-DAYS.
           GO TO CREATE-ROUTINE.
       A-20.
           MOVE VDUBUF TO SP-EXPD-EXCEPT.
           IF NOT SP-EXPD-EXCEPT-VALID
              MOVE "ENTER: SPACE OR 'A'" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       A-21.
           MOVE VDUBUF TO SP-EXPR-FRMLA.
           IF NOT SP-EXPR-FRMLA-VALID
              MOVE "ENTER: SPACE OR 'A' OR 'B' OR 'C' " TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           PERFORM VALIDATE-EXPR-FRMLA.
           GO TO CREATE-ROUTINE.
       A-22.
           MOVE VDUBUF TO SP-EXPR-LCHG.
           IF NOT SP-EXPR-LCHG-VALID
              MOVE "ENTER: SPACE ONLY" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       A-23.
           MOVE VDUBUF TO SP-EXPR-REDU-FRMLA.
           IF NOT SP-EXPR-REDU-FRMLA-VALID
              MOVE "ENTER: SPACE OR 'A'" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           PERFORM VALIDATE-EXPR-REDU-FRMLA.
           GO TO CREATE-ROUTINE.
       A-24.
           PERFORM ENTER-RATE-TABLE.
           GO TO CREATE-ROUTINE.
       A-25.
           PERFORM ENTER-RATE-TABLE.
           GO TO CREATE-ROUTINE.
       A-26.
           PERFORM ENTER-RATE-TABLE.
           GO TO CREATE-ROUTINE.
       A-27.
           PERFORM ENTER-RATE-TABLE.
           GO TO CREATE-ROUTINE.
       A-28.
           PERFORM ENTER-RATE-TABLE.
           GO TO CREATE-ROUTINE.
       END-ENTRY-MODULE.
           MOVE 0 TO FORM-SEL1 FORM-SEL2.
       EXIT-ENTRY-MODULE.
           EXIT.

      ************************************************
       ENTER-RATE-TABLE SECTION.
      ************************************************

      * ADJUST ELE TO SUBSCRIPT TABLE

           SUBTRACT 23 FROM ELE GIVING SUB.
           MOVE VDUNUM3 TO SP-EXPR-RATE(SUB).
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
              MOVE SP-EXPR-BREAK(SUB) TO VDUNUM2
              MOVE "00" TO VDUSTATUS.
           IF VDUSTATUS NOT = "00"
              GO TO ENTER-AMOUNT.
           MOVE VDUNUM2 TO SP-EXPR-BREAK(SUB).
       ENTER-YEARTYPE.
           MOVE "ENNN030  2." TO VDU.
           MOVE ELE TO VDUROW.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-AMOUNT.
           IF VDUSTATUS = "1D"
              MOVE SP-EXPR-YRTYPE(SUB) TO VDUNUM0
              MOVE "00" TO VDUSTATUS.
           IF VDUSTATUS NOT = "00"
              GO TO ENTER-YEARTYPE.
           MOVE VDUNUM0 TO SP-EXPR-YRTYPE(SUB).
           IF NOT SP-EXPR-YRTYPE-VALID(SUB)
              MOVE "ENTER: 358, 360, 365, 367 OR 999" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-YEARTYPE.
       ENTER-INTMETHOD.
           MOVE "E  A010  3." TO VDU.
           MOVE ELE TO VDUROW.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-YEARTYPE.
           IF VDUSTATUS = "1D"
              MOVE SP-EXPR-INTMETH(SUB) TO VDUBUF
              MOVE "00" TO VDUSTATUS.
           IF VDUSTATUS NOT = "00"
              GO TO ENTER-INTMETHOD.
           MOVE VDUBUF TO SP-EXPR-INTMETH(SUB).
           IF NOT SP-EXPR-INTMETH-VALID(SUB)
              MOVE "ENTER: 'E' OR 'O'" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-INTMETHOD.
       ENTER-RATE-TABLE-EXIT.
           EXIT.

      ************************************************
       CLEAR-RATE-TABLE SECTION.
      ************************************************

      * CLEAR INT TABLE FROM S TO END WHEN 0 IS ENTERED

       CLEAR-RATES.
           MOVE "S  A000  0." TO VDU.
           MOVE ELE TO VDUROW.
           PERFORM SCREENIO.
           MOVE 1 TO VDUCOL.
           PERFORM SCREENIO.
           MOVE 2 TO VDUCOL.
           PERFORM SCREENIO.
           MOVE 3 TO VDUCOL.
           PERFORM SCREENIO.
           MOVE 0 TO SP-EXPR-RATE(SUB) SP-EXPR-BREAK(SUB).
           MOVE 0 TO SP-EXPR-YRTYPE(SUB).
           MOVE " " TO SP-EXPR-INTMETH(SUB).
           IF SUB < 5
              ADD 1 TO SUB ELE
              GO TO CLEAR-RATES.
       CLEAR-RATE-TABLE-EXIT.
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
           MOVE SP-MIN-CREDIT-SCORE TO A01.
           MOVE SP-PACESETTER-FG TO A02.
           MOVE SP-WAGE-ASSIGN-DAYS TO A03.
           MOVE SP-CAL-PAYMENT-ROUND-FG TO A04.
           MOVE SP-RENEWABLE-PAYS TO A05.
           MOVE SP-RENEWABLE-DAYS TO A06.
           IF SP-MAX-CASH-PROCEEDS = 202020.20 OR 020202.02
              MOVE 999999.99 TO A07
           ELSE
              MOVE SP-MAX-CASH-PROCEEDS TO A07.
           PERFORM DISPLAY-SUBCLASS-DESC.
           MOVE SP-EXPD-FRMLA TO A17.
           MOVE SP-EXPD-MONTHS TO A18.
           MOVE SP-EXPD-DAYS TO A19.
           MOVE SP-EXPD-EXCEPT TO A20.
           MOVE SP-EXPR-FRMLA TO A21.
           MOVE SP-EXPR-LCHG TO A22.
           MOVE SP-EXPR-REDU-FRMLA TO A23.
           MOVE SP-EXPR-RATE(1) TO A24.
           MOVE SP-EXPR-BREAK(1) TO A241.
           MOVE SP-EXPR-YRTYPE(1) TO A242.
           MOVE SP-EXPR-INTMETH(1) TO A243.
           MOVE SP-EXPR-RATE(2) TO A25.
           MOVE SP-EXPR-BREAK(2) TO A251.
           MOVE SP-EXPR-YRTYPE(2) TO A252.
           MOVE SP-EXPR-INTMETH(2) TO A253.
           MOVE SP-EXPR-RATE(3) TO A26.
           MOVE SP-EXPR-BREAK(3) TO A261.
           MOVE SP-EXPR-YRTYPE(3) TO A262.
           MOVE SP-EXPR-INTMETH(3) TO A263.
           MOVE SP-EXPR-RATE(4) TO A27.
           MOVE SP-EXPR-BREAK(4) TO A271.
           MOVE SP-EXPR-YRTYPE(4) TO A272.
           MOVE SP-EXPR-INTMETH(4) TO A273.
           MOVE SP-EXPR-RATE(5) TO A28.
           MOVE SP-EXPR-BREAK(5) TO A281.
           MOVE SP-EXPR-YRTYPE(5) TO A282.
           MOVE SP-EXPR-INTMETH(5) TO A283.
           MOVE DISPLAY-BUF TO WR-BUF.
           MOVE DISPLAY-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
           PERFORM VALIDATE-EXPD-FRMLA.
           PERFORM VALIDATE-EXPR-FRMLA.
           PERFORM VALIDATE-EXPR-REDU-FRMLA.
       DISPLAY-FIELDS-EXIT.
           EXIT.

      ************************************************
       FORM-RESET SECTION.
      ************************************************
           MOVE EXT-CONAME-CONAME TO SPMANK-CONAME.
           MOVE EXT-CONAME-SYSDATE TO SPMANK-SYSDATE.
           DISPLAY SPACES UPON SPMAIN-WINDOW-HANDLE
                               LINE 0 COL 0 ERASE SCREEN.
           DISPLAY SPMANK-SCREEN UPON SPMAIN-WINDOW-HANDLE.
           MOVE SPMANK-FORM-FIELDS TO WR-BUF.
           MOVE SPMANK-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      ************************************************
       SETUP-LINK-INFO SECTION.
      ************************************************
           MOVE SPMANK-HELP--FILE TO HELP-LINK-FILE.
           MOVE SPMANK-HELP--DIR TO HELP-LINK-DIR.

      ************************************************
       VDU-CONVERT-FIELD SECTION.
      ************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBMN/SPMANK_EVA.CPY".
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
       DISPLAY-SUBCLASS-DESC.
           MOVE "S  A150DESC. " TO VDU.
           MOVE SP-DESC TO VDUBUF.
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
       VALIDATE-EXPD-FRMLA.
           MOVE "S  A130171." TO VDU.
           MOVE "INVALID" TO VDUBUF.
           IF SP-EXPD-FRMLA = " "
              MOVE "UPDATED MATUR" TO VDUBUF.
           IF SP-EXPD-FRMLA = "A" OR "B"
              MOVE "ORIG. MATURTY" TO VDUBUF.
           PERFORM SCREENIO.
       VALIDATE-EXPR-FRMLA.
           MOVE "S  A130211." TO VDU.
           MOVE "INVALID" TO VDUBUF.
           IF SP-EXPR-FRMLA = " "
              MOVE "CONT PERCONTR" TO VDUBUF.
           IF SP-EXPR-FRMLA = "A"
              MOVE "TERM EXPIRTBL" TO VDUBUF.
           IF SP-EXPR-FRMLA = "B"
              MOVE "OVRATE<EXPRTB" TO VDUBUF.
           IF SP-EXPR-FRMLA = "C"
              MOVE "LOW EXPR/SMP" TO VDUBUF.
           PERFORM SCREENIO.
       VALIDATE-EXPR-REDU-FRMLA.
           MOVE "S  A130231." TO VDU.
           MOVE "INVALID" TO VDUBUF.
           IF SP-EXPR-REDU-FRMLA = " "
              MOVE "CONT. REDURAT" TO VDUBUF.
           IF SP-EXPR-REDU-FRMLA = "A"
              MOVE "LOW EXPR/REDU" TO VDUBUF.
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

      ************************************************
       IO-ROUTINES SECTION.
      ************************************************
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

      ************************************************
       END-ROUTINE SECTION.
      ************************************************
           PERFORM CLOSE-FILES.
       EXIT-PROG.
       STOP-RUN.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY SPMANK-SCREEN.
           MOVE FORM-PATH TO FORM-PATHNAME.
           MOVE SPMAIN-LINK TO FORM-LINK-AREA.
           EXIT PROGRAM.
