       IDENTIFICATION DIVISION.
       PROGRAM-ID.      SPMAN4.
      **************************************************************************
      *   DIR :  MN
      *   DESC:  STATE PARAMETER FILE MAINT - PROG 4
      *
      *REV:
      * JTG 080889 ALLOWED SP-CAL-TBFRMLA 1 WAGNER (NC)
      * SLC 032290 MODIFIED FOR ADDONS
      * JTG 051690 ALLOWED SP-CAL-TBFRMLA 2 AND 3 (PACESETTER)
      * BAH 090490 ALLOWED SP-CAL-CHGABLE 'I' ON IB'S FOR COLONIAL
      * JTG 031491 ALLOWED SP-CAL-CHGABLE 'S' ON IB'S FOR ADVANCED
      * JTG 070191 STOPPED CAL RATES FROM BEING > 99.999
      * BAH 082291 ADDED AMTYPE "A" ADJ PRINICPAL FOR MI
      * BAH 022892 REMOVED ADDON JUNK
      * MJD 050895 ADDED CODE TO MAKE CHANGE FILE AFTER REWRITE
      * JFF 120195 ADDED LOAN SCAN & STATE PARAMETER SCAN
      *  CS 042398 CHANGED EDIT ON SP-CALC-RATE FROM 99.999 TO 999.999
      * BLV 061198 CHANGE ORIG, CHANGED RECS TO USE SP-SIZE(4513) NOT 4502
      * JTG 100598 ADDED SP-CAL-BREAK-ON-FLATAMT    VALUE "F", WORLD 'MO'
      * JTG 990113 ADDED SP-EDT-TYPE 'P' POINTS,               REGENCY #012
      * BLV 000503 MOVE 0 TO SPSCAN-SUBCLASS WHEN SCANNING FROM CLASS - WHEN
      *            FIELD NOT INITIALIZED CAUSED PROBLEMS IN SCAN & MADE
      *            NON-NUMERIC ERRORS IN ACU_TCLP
      * JTG 000619 ADDED LOGIC FOR TENNESSEE RATE TYPE 'M' FLAT MONTHLY CHARGE
      *            #1225
      * MJD 000928 ADDED F2-MEMO OPTION FOR NEW SPR MEMO FILE (SMFILE)
      * JTG 010112 ADDED NEW 88 SP-CAL-SET-SMPRATE-OVRRIDE VALUE 4 MERC PR#J016
      * JTG 010228 ADDED NEW RATE TYPE 'Z' AMORTIZED WHERE RUNTIME COMPUTES
      *            INTEREST ON A 30 DAY METHOD (REAL ESTATE) AND LOAN CALCS
      *            ARE DONE SIMPLE                   LENDMARK PR#1045 & PR#1380
      * JTG 010417 CHANGED NEW RATE TYPE 'Z' AMORTIZED TO NOT BE ALLOWED WHEN
      *            TOTAL MONEY LATE CHARGE FORMULA EXISTS
      *                                              LENDMARK PR#1045 & PR#1380
      * JTG 010718 CHANGED NEW RATE TYPE 'Z' AMORTIZED TO NOT BE ALLOWED UNLESS
      *            THE LOAN CLASS IS 'IB'            LENDMARK PR#1045 & PR#1380
      * JTG 010912 NEW SP-CAL-TBFRMLA (5) SP-FORCE-ENTRY-OVRDRATE CENTRIX #1592
      * JTG 020603 CHANGED FOR (MIP) MONTHLY INSURANCE PREMIUMS   REGENCY #1801
      * JTG 031202 ADDED FOR SIMPLE RATES SP-CAL-BREAK-ON-FLATAMT "F" WORLD J038
      * JTG 040607 ADDED EDIT ON ENTRY OF TABLE FORMULA '1' OR '2' TO NOT
      *            ALLOW THESE FORMULAS WHEN RATE TYPE IS 'A' ADDON OR
      *            'D' DISCOUNT. BOTH THESE RATE TYPES SET THE EFFECTIVE RATE
      *            TO ADDON/DISCOUNT WHICH IS NOT A VALID OVERRIDE
      *            RATE FOR PAYMENT POSTING                       SUPPORT #2313
      *  CS 041004 ADD CALL TO CL/SPMEMO WHICH WRITES A SPR CHANGE MEMO
      * JTG 060504 CHANGED LOGIC FOR SINGLE PAYS LOANS > 11 MONTHS
      *            TO YIELD REMAINING DAYS FROM UPER-INTDATE PLUS
      *            FULL UNIT PERIODS TO UPER-1STPYDATE
      *            ADDED NEW LOAN CALCULATION FOR RICHARD        LENDMARK #2709
      *            ADDED 88 SP-CAL-ONEPAY-GT-1YR
      *  CS 160728 CHANGED SAVEKEY FROM X(8) TO X(9)
      *  CS 170505 ACTIVATE 8 DIGIT DATES
      * BAH 170516 ADDED SP-EDT-AMTYPE "R" TOTAL REPAY FOR REGMGM PR#5291
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
           "@(#)MN/SPMAN4.CBL S34 06/06/21-16:55:27 >".
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
       01  S                 PIC 99     COMP-3.
       01  ELE               PIC 99     VALUE 0.
       01  ELE-OVER-20       PIC 99     VALUE 0.
       01  VALID-BREAK       PIC X.
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

      *    **********************************
      *    *      SCREEN DISPLAY BUFFER
      *    **********************************
       01  DISPLAY-BUF     VALUE SPACES.
           03  F01             PIC ZZ.
           03  F02             PIC X.
           03  F03             PIC X.
           03  F04             PIC X.
           03  F05             PIC ZZ.
           03  INT-RATE1       OCCURS 5.
               05  I1-RATE     PIC ZZ9.999   BLANK ZERO.
               05  I1-AMT      PIC ZZZZZ9.99 BLANK ZERO.
           03  INT-EDIT1       OCCURS 6.
               05  E1-EDITS    PIC X(5).
               05  E1-RATETY   PIC X.
               05  E1-RATE     PIC Z9.99     BLANK ZERO.
               05  E1-AMTTY    PIC X.
               05  E1-AMT      PIC ZZZZZ9.99 BLANK ZERO.
               05  E1-TERM     PIC ZZ9.9     BLANK ZERO.
           03  F17             PIC X.
           03  F18             PIC X.
           03  F19             PIC X.
           03  F20             PIC ZZ.
           03  INT-RATE2       OCCURS 5.
               05  I2-RATE     PIC ZZ9.999   BLANK ZERO.
               05  I2-AMT      PIC ZZZZZ9.99 BLANK ZERO.
           03  INT-EDIT2       OCCURS 6.
               05  E2-EDITS    PIC X(5).
               05  E2-RATETY   PIC X.
               05  E2-RATE     PIC Z9.99     BLANK ZERO.
               05  E2-AMTTY    PIC X.
               05  E2-AMT      PIC ZZZZZ9.99 BLANK ZERO.
               05  E2-TERM     PIC ZZ9.9     BLANK ZERO.

       01  DISPLAY-LISTS.
           03  FILLER        PIC X(44)  VALUE
           "010 020 030 040 050 060 061 070 071 080 081 ".
           03  FILLER        PIC X(40)  VALUE
           "090 091 100 101 110 111 112 113 114 115 ".
           03  FILLER        PIC X(48)  VALUE
           "120 121 122 123 124 125 130 131 132 133 134 135 ".
           03  FILLER        PIC X(48)  VALUE
           "140 141 142 143 144 145 150 151 152 153 154 155 ".
           03  FILLER        PIC X(40)  VALUE
           "160 161 162 163 164 165 170 180 190 200 ".
           03  FILLER        PIC X(40)  VALUE
           "210 211 220 221 230 231 240 241 250 251 ".
           03  FILLER        PIC X(48)  VALUE
           "260 261 262 263 264 265 270 271 272 273 274 275 ".
           03  FILLER        PIC X(48)  VALUE
           "280 281 282 283 284 285 290 291 292 293 294 295 ".
           03  FILLER        PIC X(48)  VALUE
           "300 301 302 303 304 305 310 311 312 313 314 315.".
       01  DISPLAY-LIST REDEFINES DISPLAY-LISTS
                             PIC X(404).

      *    **********************************
      *    *   ELEMENT SPECIFICATIONS
      *    **********************************
       01  MAX              PIC 99 COMP-3 VALUE 31.
       01  SPEC-TABLE.
           03  FILLER        PIC X(8)   VALUE "ENNN020 ".
           03  FILLER        PIC X(8)   VALUE "E  A010 ".
           03  FILLER        PIC X(8)   VALUE "E  A010 ".
           03  FILLER        PIC X(8)   VALUE "E  A010 ".
      * 5. SP-CAL-TBFRMLA(1)
           03  FILLER        PIC X(8)   VALUE "ENNN020 ".
      * 6. SP-CAL-RATE(1)
           03  FILLER        PIC X(8)   VALUE "ENNN033 ".
           03  FILLER        PIC X(8)   VALUE "ENNN033 ".
           03  FILLER        PIC X(8)   VALUE "ENNN033 ".
           03  FILLER        PIC X(8)   VALUE "ENNN033 ".
      * 10. SP-CAL-RATE(5)
           03  FILLER        PIC X(8)   VALUE "ENNN033 ".
           03  FILLER        PIC X(8)   VALUE "E  A050 ".
           03  FILLER        PIC X(8)   VALUE "E  A050 ".
           03  FILLER        PIC X(8)   VALUE "E  A050 ".
           03  FILLER        PIC X(8)   VALUE "E  A050 ".
      * 15. SP-EDT-TYPE-CODES(5)
           03  FILLER        PIC X(8)   VALUE "E  A050 ".
           03  FILLER        PIC X(8)   VALUE "E  A050 ".
           03  FILLER        PIC X(8)   VALUE "E  A010 ".
           03  FILLER        PIC X(8)   VALUE "E  A010 ".
           03  FILLER        PIC X(8)   VALUE "E  A010 ".
      * 20. SP-CAL-TBFRMLA(2)
           03  FILLER        PIC X(8)   VALUE "ENNN020 ".
      * 21. SP-CAL-RATE(1)
           03  FILLER        PIC X(8)   VALUE "ENNN033 ".
           03  FILLER        PIC X(8)   VALUE "ENNN033 ".
           03  FILLER        PIC X(8)   VALUE "ENNN033 ".
           03  FILLER        PIC X(8)   VALUE "ENNN033 ".
      * 25. SP-CAL-RATE(5)
           03  FILLER        PIC X(8)   VALUE "ENNN033 ".
           03  FILLER        PIC X(8)   VALUE "E  A050 ".
           03  FILLER        PIC X(8)   VALUE "E  A050 ".
           03  FILLER        PIC X(8)   VALUE "E  A050 ".
           03  FILLER        PIC X(8)   VALUE "E  A050 ".
      * 30. SP-EDT-TYPE-CODES(5)
           03  FILLER        PIC X(8)   VALUE "E  A050 ".
           03  FILLER        PIC X(8)   VALUE "E  A050 ".
       01  SPEC-TAB       REDEFINES SPEC-TABLE.
           03  SPEC-REC3   OCCURS 31 TIMES.
               05  SPEC      PIC X(7).
               05  CHFG      PIC X.

       COPY "LIBWI/SPSCANW.CPY".
       COPY "LIBCL/CLMENUW_EXT.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBWI/MEMOSPW.CPY".
       COPY "LIBWI/LCSCANW.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBMN/SPMAN4_DEF.CPY".
       COPY "LIBMN/SPMAN4_WKS.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/GPENVW.CPY".
       COPY "LIBGB/ACCESSW.CPY".

       COPY "LIBGB/GETFMW.CPY".
       01 FORM-LINK-AREA    PIC X(2000).

       SCREEN SECTION.
       COPY "LIBMN/SPMAN4_SCN.CPY".

       PROCEDURE DIVISION
            USING FORM-PATHNAME FORM-LINK-AREA EXIT-PATHNAME.

      ************************************
      *       MAIN PROGRAM MODULE
      ************************************
       MAIN-PROGRAM SECTION.
           PERFORM SQL-CONNECT.

       COPY "LIBGB/CHKSEC.CPY".

           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "SPMAN4" TO VDU-FORM-NAME.

           MOVE " " TO VDU-WIMENU-SELECTION
                       VDU-PGMENU-SELECTION.

           MOVE FORM-LINK-AREA TO SPMAIN-LINK.

           PERFORM GET-GPENV.

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
              MOVE 5 TO FORM-SEL1.
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

      ****************************************
       REC-SP-MODULE SECTION.
      ****************************************
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
           MOVE "LCDESC." TO WR-BUF.
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
              MOVE SP-ORGST    TO SPSCAN-ORGST
              MOVE SP-SPRCLASS TO SPSCAN-SPRCLASS
              MOVE 0           TO SPSCAN-SUBCLASS
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
           PERFORM OPEN-SP1-FILE.
           PERFORM READ-SP1-FILE.
           PERFORM CLOSE-SP1-FILE.
           IF IO-FG NOT = 0
              GO TO REC-SP-MODULE-NO.
           MOVE SP-REC TO ORIGINAL-SP-REC
           PERFORM DISPLAY-FIELDS
           MOVE 0 TO NEW-FLAG.
           IF NEW-REC = "Y"
              MOVE 1 TO NEW-FLAG.
       END-REC-SP-MODULE.
           EXIT.

      ****************************************
       CLASS-SCAN SECTION.
      ****************************************
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

      ****************************************
       ENTER-SCANKEY SECTION.
      ****************************************
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
              PERFORM CHECK-EDIT-TABLE-BREAK
              IF VALID-BREAK = "N"
                 GO TO MODIFY-ROUTINE
              ELSE
                 MOVE "RWR" TO IO-TYPE
                 GO TO END-ENTRY-MODULE.
      * FORM-NUM1 CHECKS FOR PAGES 1 THRU 9
           IF FORM-KEY1 = " " AND FORM-P1 = "P" AND FORM-SEL1 NUMERIC
              IF FORM-SEL1 < 1 OR FORM-SEL1 = 4
                 GO TO MODIFY-ROUTINE
              ELSE
                 GO TO CHECK-PAGE.
      * FORM-NUM2 CHECKS FOR PAGE 14
           IF FORM-KEY2 = " " AND FORM-P2 = "P" AND FORM-SEL2 NUMERIC
              IF NOT FORM-2-DIGITS
                 GO TO MODIFY-ROUTINE
              ELSE
                 GO TO CHECK-PAGE.
           GO TO CHK-CHNO.
       CHECK-PAGE.
           PERFORM CHECK-EDIT-TABLE-BREAK.
           IF VALID-BREAK = "N"
              GO TO MODIFY-ROUTINE.
           MOVE "RWR" TO IO-TYPE.
           GO TO EXIT-ENTRY-MODULE.
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
           GO TO F-01 F-02 F-03 F-04 F-05
                 F-06 F-07 F-08 F-09 F-10
                 F-11 F-12 F-13 F-14 F-15
                 F-16 F-17 F-18 F-19 F-20
                 DEPENDING ON ELE.
           GO TO END-ENTRY-MODULE.
       OVER-20.
           GO TO F-21 F-22 F-23 F-24 F-25
                 F-26 F-27 F-28 F-29 F-30
                 F-31
                 DEPENDING ON ELE-OVER-20.
           GO TO END-ENTRY-MODULE.

      ***************************
      *    SET RECORDS FIELDS
      ***************************
       F-01.
           MOVE VDUNUM0 TO SP-CAL-SPFRMLA.
           IF NOT SP-CAL-SPFRMLA-VALID
              MOVE "ENTER: 0 OR 1" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       F-02.
           MOVE VDUBUF TO SP-CAL-RATETYPE(1).
           IF NOT SP-CAL-RATETYPE-VALID(1)
              MOVE "ENTER: SPACE, 'A', 'D', 'S', 'M' OR 'Z' OR 'G'"
                                                            TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           IF NEW-FLAG NOT = 1
              IF SP-CAL-TBFRMLA(1) = 1 OR 2
                 IF SP-CAL-RATETYPE(1) = "A" OR "D"
                    MOVE "NOT ALLOWED WITH TABLE FORMULA 1 OR 2" TO MESS
                    PERFORM SEND-MESS
                    GO TO ENTER-ELE.
           IF NEW-FLAG NOT = 1
              IF SP-CAL-FLAT-MONTHLY(1)
                 IF SP-CAL-BREAK-ON-FLATAMT(1)
                    STRING "FLAT MONTHLY RATES NOT"
                           " ALLOWED WITH FLAT STEP BREAKS!"
                           DELIMITED BY SIZE INTO MESS
                   PERFORM SEND-MESS
                   GO TO ENTER-ELE.
           IF (SP-CAL-RATETYPE(1) = "Z") AND
              (LC-LOANTYPE NOT = "I")
              STRING "RATE TYPE 'Z' ONLY ALLOWED FOR INTEREST"
                     " BEARING LOAN CLASSES"
                     DELIMITED BY SIZE INTO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.

           IF (SP-CAL-RATETYPE(1) = "Z") AND
              (SP-CONTRFRMLA = "A" OR SP-LCFRMLA-TOTAL-MONEY)
              STRING "TYPE 'Z' NOT ALLOWED WITH TOTAL MONIES "
                     "CONTRACTUAL FORMULA"
                     DELIMITED BY SIZE INTO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.

           IF (SP-CAL-RATETYPE(1) = "Z") AND
              (GP-ALLOW-MORTGAGE-INT-Z NOT = "Y")
              STRING "TYPE 'Z' IS NOT ALLOWED"
                     DELIMITED BY SIZE INTO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.

           IF (SP-CAL-RATETYPE(1) = "G") AND
              (LC-LOANTYPE NOT = "I")
              STRING "RATE TYPE 'G' ONLY ALLOWED FOR INTEREST"
                     " BEARING LOAN CLASSES"
                     DELIMITED BY SIZE INTO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.

           GO TO CREATE-ROUTINE.
       F-03.
           MOVE VDUBUF TO SP-CAL-BREAKTYPE(1).
           IF NOT SP-CAL-BREAKTYPE-VALID(1)
              MOVE "ENTER: SPACE, 'A', 'T' OR 'F'" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           IF SP-CAL-BREAK-ON-FLATAMT(1)
              IF SP-CAL-FLAT-MONTHLY(1)
                 STRING "FLAT MONTHLY RATES NOT"
                        " ALLOWED WITH FLAT STEP BREAKS!"
                        DELIMITED BY SIZE INTO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       F-04.
           MOVE VDUBUF TO SP-CAL-CHGABLE(1).
           IF NOT SP-CAL-CHGABLE-VALID(1)
              MOVE "ENTER: 'P', 'I', 'S', OR 'B'" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           IF SP-CAL-CHGABLE(1) = "B" OR "I"
              IF SP-INSCHGABL(1) = "Y" OR
                 SP-INSCHGABL(2) = "Y" OR
                 SP-INSCHGABL(3) = "Y" OR
                 SP-INSCHGABL(4) = "Y" OR
                 SP-INSCHGABL(5) = "Y"
                 MOVE
           "CHARGEABLE MUST BE 'P' OR 'S' WHEN INS. CHARGABLE='Y' MIP"
                                                          TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       F-05.
           MOVE VDUNUM0 TO SP-CAL-TBFRMLA(1).
           IF NOT SP-CAL-TBFRMLA-VALID(1)
              MOVE "ENTER: 0 THRU 5 ONLY" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           IF SP-CAL-TBFRMLA(1) = 1 OR 2
              IF SP-CAL-RATETYPE(1) = "A" OR "D"
                 MOVE "NOT ALLOWED WITH RATE TYPE 'A' OR 'D'" TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-ELE.
           IF SP-CAL-TBFRMLA(1) = 5
              IF (LC-LOANTYPE NOT = "I") OR (LC-SALEFIN NOT = "Y")
                 MOVE "LOAN CLASS MUST BE SALES FINANCE & TYPE IB"
                                                           TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       F-06.
           MOVE 1 TO SUB.
           PERFORM ENTER-RATE-TABLE.
           IF IO-FG = 9
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       F-07.
           MOVE 1 TO SUB.
           PERFORM ENTER-RATE-TABLE.
           IF IO-FG = 9
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       F-08.
           MOVE 1 TO SUB.
           PERFORM ENTER-RATE-TABLE.
           IF IO-FG = 9
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       F-09.
           MOVE 1 TO SUB.
           PERFORM ENTER-RATE-TABLE.
           IF IO-FG = 9
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       F-10.
           MOVE 1 TO SUB.
           PERFORM ENTER-RATE-TABLE.
           IF IO-FG = 9
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       F-11.
           MOVE 1 TO SUB.
           PERFORM ENTER-EDIT-TABLE.
           IF IO-FG = 9
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       F-12.
           MOVE 1 TO SUB.
           PERFORM ENTER-EDIT-TABLE.
           IF IO-FG = 9
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       F-13.
           MOVE 1 TO SUB.
           PERFORM ENTER-EDIT-TABLE.
           IF IO-FG = 9
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       F-14.
           MOVE 1 TO SUB.
           PERFORM ENTER-EDIT-TABLE.
           IF IO-FG = 9
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       F-15.
           MOVE 1 TO SUB.
           PERFORM ENTER-EDIT-TABLE.
           IF IO-FG = 9
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       F-16.
           MOVE 1 TO SUB.
           PERFORM ENTER-EDIT-TABLE.
           IF IO-FG = 9
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       F-17.
           MOVE VDUBUF TO SP-CAL-RATETYPE(2).
           IF NOT SP-CAL-RATETYPE-VALID(2)
              MOVE "ENTER: SPACE, 'A', 'D', OR 'S'" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           IF NEW-FLAG NOT = 1
              IF SP-CAL-TBFRMLA(2) = 1 OR 2
                 IF SP-CAL-RATETYPE(2) = "A" OR "D"
                    MOVE "NOT ALLOWED WITH TABLE FORMULA 1 OR 2" TO MESS
                    PERFORM SEND-MESS
                    GO TO ENTER-ELE.
      * THIS WORKS! NOT SURE WHY THIS WAS HERE. REMOVED 5/27/2021 PER KM
      *    IF SP-CAL-FLAT-MONTHLY(2)
      *       STRING "FLAT MONTHLY RATES ARE ONLY"
      *              " ALLOWED IN TABLE #1!"
      *              DELIMITED BY SIZE INTO MESS
      *          PERFORM SEND-MESS
      *          GO TO ENTER-ELE.
           IF SP-CAL-RATETYPE(2) = "Z"
              STRING "MORTGAGE INTEREST RATES ARE"
                     " ONLY ALLOWED IN TABLE #1!"
                     DELIMITED BY SIZE INTO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       F-18.
           MOVE VDUBUF TO SP-CAL-BREAKTYPE(2).
           IF NOT SP-CAL-BREAKTYPE-VALID(2)
              MOVE "ENTER: SPACE, 'A', 'T' OR 'F'" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       F-19.
           MOVE VDUBUF TO SP-CAL-CHGABLE(2).
           IF NOT SP-CAL-CHGABLE-VALID(2)
              MOVE "ENTER: 'P', 'I', 'S', OR 'B'" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           IF SP-CAL-CHGABLE(2) = "B" OR "I"
              IF SP-INSCHGABL(1) = "Y" OR
                 SP-INSCHGABL(2) = "Y" OR
                 SP-INSCHGABL(3) = "Y" OR
                 SP-INSCHGABL(4) = "Y" OR
                 SP-INSCHGABL(5) = "Y"
                 MOVE
           "CHARGEABLE MUST BE 'P' OR 'S' WHEN INS. CHARGABLE='Y' MIP"
                                                          TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       F-20.
           MOVE VDUNUM0 TO SP-CAL-TBFRMLA(2).
           IF NOT SP-CAL-TBFRMLA-VALID(2)
              MOVE "ENTER: 0 THRU 4 ONLY" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           IF SP-CAL-TBFRMLA(2) = 1 OR 2
              IF SP-CAL-RATETYPE(2) = "A" OR "D"
                 MOVE "NOT ALLOWED WITH RATE TYPE 'A' OR 'D'" TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-ELE.
           IF SP-CAL-TBFRMLA(2) = 5
              MOVE "RATE TABLE FORMULA 5 IS NOT VALID FOR TABLE 2"
                                                           TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       F-21.
           MOVE 2 TO SUB.
           PERFORM ENTER-RATE-TABLE.
           IF IO-FG = 9
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       F-22.
           MOVE 2 TO SUB.
           PERFORM ENTER-RATE-TABLE.
           IF IO-FG = 9
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       F-23.
           MOVE 2 TO SUB.
           PERFORM ENTER-RATE-TABLE.
           IF IO-FG = 9
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       F-24.
           MOVE 2 TO SUB.
           PERFORM ENTER-RATE-TABLE.
           IF IO-FG = 9
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       F-25.
           MOVE 2 TO SUB.
           PERFORM ENTER-RATE-TABLE.
           IF IO-FG = 9
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       F-26.
           MOVE 2 TO SUB.
           PERFORM ENTER-EDIT-TABLE.
           IF IO-FG = 9
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       F-27.
           MOVE 2 TO SUB.
           PERFORM ENTER-EDIT-TABLE.
           IF IO-FG = 9
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       F-28.
           MOVE 2 TO SUB.
           PERFORM ENTER-EDIT-TABLE.
           IF IO-FG = 9
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       F-29.
           MOVE 2 TO SUB.
           PERFORM ENTER-EDIT-TABLE.
           IF IO-FG = 9
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       F-30.
           MOVE 2 TO SUB.
           PERFORM ENTER-EDIT-TABLE.
           IF IO-FG = 9
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       F-31.
           MOVE 2 TO SUB.
           PERFORM ENTER-EDIT-TABLE.
           IF IO-FG = 9
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       END-ENTRY-MODULE.
           MOVE 0 TO FORM-SEL1 FORM-SEL2.
       EXIT-ENTRY-MODULE.
           EXIT.

      ******************************************
       ENTER-RATE-TABLE SECTION.
      ******************************************
           MOVE 0 TO IO-FG.
           IF VDUNUM3 > 999.999
              MOVE 9 TO IO-FG
              GO TO ENTER-RATE-TABLE-EXIT.

      * ADJUST ELE TO SUBSCRIPT TABLE
           IF SUB = 1
              SUBTRACT 5 FROM ELE GIVING S
           ELSE
              SUBTRACT 20 FROM ELE GIVING S.
           MOVE VDUNUM3 TO SP-CAL-RATE(SUB S).
           IF VDUNUM3 = 0
              PERFORM CLEAR-RATE-TABLE
              GO TO ENTER-RATE-TABLE-EXIT.
       ENTER-AMOUNT.
           MOVE "ENNN062  1." TO VDU.
           MOVE ELE TO VDUROW.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              MOVE 9 TO IO-FG
              GO TO ENTER-RATE-TABLE-EXIT.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO ENTER-AMOUNT.
           MOVE VDUNUM2 TO SP-CAL-BREAK(SUB S).
       ENTER-RATE-TABLE-EXIT.
           EXIT.

      ******************************************
       CHECK-EDIT-TABLE-BREAK SECTION.
      ******************************************
           MOVE " " TO VALID-BREAK.
           IF SP-CAL-SPFRMLA NOT = 1
              GO TO CHECK-TABLE-BREAK-EXIT.

       CHECK-LAST-BREAK.
           PERFORM COMPUTE-NORATES.
           SET CTBL TO 1.
           SET CSTP TO SP-CAL-NORATES(CTBL).
           IF SP-CAL-BREAK(CTBL CSTP) NOT = 0
              MOVE "N" TO VALID-BREAK
              MOVE "LAST BREAK MUST BE 0 FOR SPECIAL FORMULA = 1"
                 TO MESS
              PERFORM SEND-MESS.
       CHECK-TABLE-BREAK-EXIT.
           EXIT.

      ******************************************
       CLEAR-RATE-TABLE SECTION.
      ******************************************
       CLEAR-RATES.

      * CLEAR INT TABLE FROM S TO END
      * WHEN 0 IS ENTERED
           MOVE "S  A000  0." TO VDU.
           MOVE ELE TO VDUROW.
           PERFORM SCREENIO.
           MOVE 1 TO VDUCOL.
           PERFORM SCREENIO.
           MOVE 0 TO SP-CAL-RATE(SUB S).
           MOVE 0 TO SP-CAL-BREAK(SUB S).
           IF S < 5
              ADD 1 TO S ELE
              GO TO CLEAR-RATES.

       CLEAR-RATE-TABLE-EXIT.
           EXIT.

      ******************************************
       ENTER-EDIT-TABLE SECTION.
      ******************************************
           MOVE 0 TO IO-FG.

      * ADJUST ELE TO SUBSCRIPT TABLE
           IF SUB = 1
              SUBTRACT 10 FROM ELE GIVING S
           ELSE
              SUBTRACT 25 FROM ELE GIVING S.
           MOVE VDUBUF TO SP-EDT-TYPE-CODES(SUB S).
           IF VDUBUF = SPACES
              PERFORM CLEAR-EDIT-TABLE
              GO TO ENTER-EDIT-TABLE-EXIT.
           IF NOT SP-EDT-TYPE-VALID(SUB S)
              MOVE
              "ENTER: 'R', 'A', 'T', 'P', 'V', OR 'M' IN FIRST POSITION"
                TO MESS
              PERFORM SEND-MESS
              MOVE 9 TO IO-FG
              GO TO ENTER-EDIT-TABLE-EXIT.
           IF SP-EDT-TYPE(SUB S) = "M"
              IF SP-EDT-TYPE-CODES(SUB S) = "M    "
                 GO TO ENTER-EDT-AMT-TYPE
              ELSE
                 MOVE "ENTER: SPACES ONLY IN SECOND - FIFTH POSITIONS"
                   TO MESS
                 PERFORM SEND-MESS
                 MOVE 9 TO IO-FG
                 GO TO ENTER-EDIT-TABLE-EXIT.
           IF SP-EDT-HL(SUB S) NOT = "H" AND
              SP-EDT-HL(SUB S) NOT = "L"
              MOVE "ENTER: 'H' OR 'L' IN SECOND POSITION" TO MESS
              PERFORM SEND-MESS
              MOVE 9 TO IO-FG
              GO TO ENTER-EDIT-TABLE-EXIT.
           IF NOT SP-EDT-COLL-VALID(SUB S)
              MOVE "ENTER: SPACES, 'S ', 'U ', OR 'R ' IN THIRD AND FOUR
      -    "TH POSITIONS" TO MESS
              PERFORM SEND-MESS
              MOVE 9 TO IO-FG
              GO TO ENTER-EDIT-TABLE-EXIT.
           IF NOT SP-EDT-CD-VALID(SUB S)
              MOVE "ENTER: SPACE ONLY IN FIFTH POSITION" TO MESS
              PERFORM SEND-MESS
              MOVE 9 TO IO-FG
              GO TO ENTER-EDIT-TABLE-EXIT.
       ENTER-EDT-RATE-TYPE.
           IF NOT (SP-EDT-TYPE(SUB S) = "R" OR "V")
              MOVE " " TO SP-EDT-RATETYPE(SUB S) VDUBUF
              MOVE 0 TO SP-EDT-RATE(SUB S)
              MOVE "S  A000  1." TO VDU
              MOVE ELE TO VDUROW
              PERFORM SCREENIO
              MOVE 2 TO VDUCOL
              PERFORM SCREENIO
              GO TO ENTER-EDT-AMT-TYPE.
           MOVE "E  A010  1." TO VDU.
           MOVE ELE TO VDUROW.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              MOVE 9 TO IO-FG
              GO TO ENTER-EDIT-TABLE-EXIT.
           IF VDUSTATUS = "1D"
              MOVE SP-EDT-RATETYPE(SUB S) TO VDUBUF
              MOVE "00" TO VDUSTATUS.
           IF VDUSTATUS NOT = "00"
              GO TO ENTER-EDT-RATE-TYPE.
           MOVE VDUBUF TO SP-EDT-RATETYPE(SUB S).
           IF NOT SP-EDT-RATETYPE-VALID(SUB S)
              MOVE "ENTER: SPACE, 'A' OR 'E'" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-EDT-RATE-TYPE.
       ENTER-EDT-RATE.
           MOVE "ENNN022  2." TO VDU.
           MOVE ELE TO VDUROW.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              MOVE 9 TO IO-FG
              GO TO ENTER-EDIT-TABLE-EXIT.
           IF VDUSTATUS = "1D"
              MOVE SP-EDT-RATE(SUB S) TO VDUNUM2
              MOVE "00" TO VDUSTATUS.
           IF VDUSTATUS NOT = "00"
              GO TO ENTER-EDT-RATE.
           MOVE VDUNUM2 TO SP-EDT-RATE(SUB S).
       ENTER-EDT-AMT-TYPE.
           IF NOT ( SP-EDT-TYPE(SUB S) = "A" OR "M" )
              MOVE " " TO SP-EDT-AMTYPE(SUB S) VDUBUF
              MOVE "S  A000  3." TO VDU
              MOVE ELE TO VDUROW
              PERFORM SCREENIO
              GO TO ENTER-EDT-AMOUNT.
           MOVE "E  A010  3." TO VDU.
           MOVE ELE TO VDUROW.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              MOVE 9 TO IO-FG
              GO TO ENTER-EDIT-TABLE-EXIT.
           IF VDUSTATUS = "1D"
              MOVE SP-EDT-AMTYPE(SUB S) TO VDUBUF
              MOVE "00" TO VDUSTATUS.
           IF VDUSTATUS NOT = "00"
              GO TO ENTER-EDT-AMT-TYPE.
           MOVE VDUBUF TO SP-EDT-AMTYPE(SUB S).
           IF NOT SP-EDT-AMTYPE-VALID(SUB S)
              MOVE "ENTER: SPACE, 'N', 'P', 'C', 'X', 'R' OR 'A'"
                   TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-EDT-AMT-TYPE.
       ENTER-EDT-AMOUNT.
           IF NOT ( SP-EDT-TYPE(SUB S) = "A" OR "M" OR "P" )
              MOVE 0 TO SP-EDT-AMT(SUB S) VDUNUM2
              MOVE "S  A000  4." TO VDU
              MOVE ELE TO VDUROW
              PERFORM SCREENIO
              GO TO ENTER-EDT-TERM.
           MOVE "ENNN062  4." TO VDU.
           MOVE ELE TO VDUROW.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              MOVE "9" TO IO-FG
              GO TO ENTER-EDIT-TABLE-EXIT.
           IF VDUSTATUS = "1D"
              MOVE SP-EDT-AMT(SUB S) TO VDUNUM2
              MOVE "00" TO VDUSTATUS.
           IF VDUSTATUS NOT = "00"
              GO TO ENTER-EDT-AMOUNT.
           MOVE VDUNUM2 TO SP-EDT-AMT(SUB S).
       ENTER-EDT-TERM.
           IF SP-EDT-TYPE(SUB S) NOT = "M" AND
              SP-EDT-TYPE(SUB S) NOT = "T"
              MOVE 0 TO SP-EDT-TERM(SUB S) VDUNUM1
              MOVE "S  A000  5." TO VDU
              MOVE ELE TO VDUROW
              PERFORM SCREENIO
              GO TO ENTER-EDIT-TABLE-EXIT.
           MOVE "ENNN031  5." TO VDU.
           MOVE ELE TO VDUROW.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              MOVE 9 TO IO-FG
              GO TO ENTER-EDIT-TABLE-EXIT.
           IF VDUSTATUS = "1D"
              MOVE SP-EDT-TERM(SUB S) TO VDUNUM1
              MOVE "00" TO VDUSTATUS.
           IF VDUSTATUS NOT = "00"
              GO TO ENTER-EDT-TERM.
           MOVE VDUNUM1 TO SP-EDT-TERM(SUB S).
       ENTER-EDIT-TABLE-EXIT.
           EXIT.

      ******************************************
       CLEAR-EDIT-TABLE SECTION.
      ******************************************
       CLEAR-EDITS.

      * CLEAR EDIT TABLE FROM S TO END WHEN 0 IS ENTERED

           MOVE "S  A000  0." TO VDU.
           MOVE ELE TO VDUROW.
           PERFORM SCREENIO.
           MOVE 1 TO VDUCOL.
           PERFORM SCREENIO.
           MOVE 2 TO VDUCOL.
           PERFORM SCREENIO.
           MOVE 3 TO VDUCOL.
           PERFORM SCREENIO.
           MOVE 4 TO VDUCOL.
           PERFORM SCREENIO.
           MOVE 5 TO VDUCOL.
           PERFORM SCREENIO.
           MOVE " " TO SP-EDT-TYPE-CODES(SUB S)
                       SP-EDT-RATETYPE(SUB S)
                       SP-EDT-AMTYPE(SUB S).
           MOVE 0 TO SP-EDT-RATE(SUB S)
                     SP-EDT-AMT(SUB S)
                     SP-EDT-TERM(SUB S).
           IF S < 6
              ADD 1 TO S ELE
              GO TO CLEAR-EDITS.

       CLEAR-EDIT-TABLE-EXIT.
           EXIT.

      ******************************************
       WRITE-MODULE SECTION.
      ******************************************
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
       DISPLAY-FIELDS SECTION.
      ******************************************
           MOVE SPACES TO DISPLAY-BUF.
           MOVE "S  8000EFFDATE." TO VDU.
           MOVE SP-EFFDATE TO VDU-DATE-YYYYMMDD.
           PERFORM SCREENIO.
           MOVE "S  8000EXPDATE." TO VDU.
           MOVE SP-EXPDATE TO VDU-DATE-YYYYMMDD.
           PERFORM SCREENIO.
           PERFORM DISPLAY-SUBCLASS-DESC.

           MOVE SP-CAL-SPFRMLA TO F01.
           MOVE SP-CAL-RATETYPE(1) TO F02.
           MOVE SP-CAL-BREAKTYPE(1) TO F03.
           MOVE SP-CAL-CHGABLE(1) TO F04.
           MOVE SP-CAL-TBFRMLA(1) TO F05.

           MOVE SP-CAL-RATETYPE(2) TO F17.
           MOVE SP-CAL-BREAKTYPE(2) TO F18.
           MOVE SP-CAL-CHGABLE(2) TO F19.
           MOVE SP-CAL-TBFRMLA(2) TO F20.

           MOVE 0 TO SUB.
       NEXT-INT-TERM.
           ADD 1 TO SUB.
           MOVE SP-CAL-RATE(1 SUB) TO I1-RATE(SUB).
           MOVE SP-CAL-BREAK(1 SUB) TO I1-AMT(SUB).
           MOVE SP-CAL-RATE(2, SUB) TO I2-RATE(SUB).
           MOVE SP-CAL-BREAK(2, SUB) TO I2-AMT(SUB).
           IF SUB < 5
              GO TO NEXT-INT-TERM.

           MOVE 0 TO SUB.
       NEXT-EDIT.
           ADD 1 TO SUB.
           MOVE SP-EDT-TYPE-CODES(1 SUB) TO E1-EDITS(SUB).
           MOVE SP-EDT-RATETYPE(1 SUB) TO E1-RATETY(SUB).
           MOVE SP-EDT-RATE(1 SUB) TO E1-RATE(SUB).
           MOVE SP-EDT-AMTYPE(1 SUB) TO E1-AMTTY(SUB).
           MOVE SP-EDT-AMT(1 SUB) TO E1-AMT(SUB).
           MOVE SP-EDT-TERM(1 SUB) TO E1-TERM(SUB).
           MOVE SP-EDT-TYPE-CODES(2 SUB) TO E2-EDITS(SUB).
           MOVE SP-EDT-RATETYPE(2 SUB) TO E2-RATETY(SUB).
           MOVE SP-EDT-RATE(2 SUB) TO E2-RATE(SUB).
           MOVE SP-EDT-AMTYPE(2 SUB) TO E2-AMTTY(SUB).
           MOVE SP-EDT-AMT(2 SUB) TO E2-AMT(SUB).
           MOVE SP-EDT-TERM(2 SUB) TO E2-TERM(SUB).
           IF SUB < 6
              GO TO NEXT-EDIT.

           MOVE DISPLAY-BUF TO WR-BUF.
           MOVE DISPLAY-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      ******************************************
       FORM-RESET SECTION.
      ******************************************
           MOVE EXT-CONAME-CONAME TO SPMAN4-CONAME.
           MOVE EXT-CONAME-SYSDATE TO SPMAN4-SYSDATE.
           DISPLAY SPACES UPON SPMAIN-WINDOW-HANDLE
                   LINE 0 COL 0 ERASE SCREEN.
           DISPLAY SPMAN4-SCREEN UPON SPMAIN-WINDOW-HANDLE.
           MOVE SPMAN4-FORM-FIELDS TO WR-BUF.
           MOVE SPMAN4-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      ******************************************
       SETUP-LINK-INFO SECTION.
      ******************************************
           MOVE SPMAN4-HELP--FILE TO HELP-LINK-FILE.
           MOVE SPMAN4-HELP--DIR TO HELP-LINK-DIR.

      ******************************************
       VDU-CONVERT-FIELD SECTION.
      ******************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBMN/SPMAN4_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

       COPY "LIBGB/GPENV.CPY".
       COPY "LIBLP/LPTBNO.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/DATER.CPY".

      **************************************
       MISC-PARAGRAPHS SECTION.
      **************************************
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

      **************************************
       IO-ROUTINES SECTION.
      **************************************
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

      **************************************
       END-ROUTINE SECTION.
      **************************************
           PERFORM CLOSE-FILES.
       EXIT-PROG.
       STOP-RUN.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY SPMAN4-SCREEN.
           MOVE FORM-PATH TO FORM-PATHNAME.
           MOVE SPMAIN-LINK TO FORM-LINK-AREA.
           EXIT PROGRAM.
