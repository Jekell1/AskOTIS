       IDENTIFICATION DIVISION.
       PROGRAM-ID.      SPMANA.
      ***********************************************************
      *   DESC:  STATE PARAMETER FILE MAINT - PROG 10
      *          PERSONAL PROPERTY INSURANCE
      *
      * REV:
      *  MJD 050895 ADDED CODE TO WRITE CHANGE FILE AFTER REWRITE
      *  MJD 071095 ADDED SP-INSREJECT-FG(6).
      *  JTG 083195 ADDED SP-INSRATECD = 5, MERCURY ACTUARIAL 78THS (SF) (VA)
      *                   SP-INSRATECD = 6, MERCURY ACTUARIAL (LOANS) (VA)
      *  JTG 091995 ADDED SP-INSEXCEPT = 12, MERCURY 'LA'
      *  MJD 092895 ADDED SP-TX-PP-FIRE, INS RATE CODE 7.
      *  JFF 120495 ADDED LOAN CLASS SCAN & STATE PARAMETER SCAN
      *  JTG 051596 CHANGED SP-INS-RATE-FILE VALUES TO 71 THRU 99
      *  JTG 111596 ADDED SP-INSRATECD = 11, (88) SP-ADVANCE-MTG-FIRE
      *  JTG 081397 ADDED SP-INSFINCHG FLAG WORLD A90
      *  BLV 061198 CHANGE ORIG, CHANGED RECS TO USE SP-SIZE(4513) NOT 4502
      *  JTG 990218 ADDED SP-INSCHGABLE = 'F', CASH ADV LESS FEES, REGENCY #020
      *  BAH 990222 DONT ALLOW SP-INSEXCEPT 13, REGENCY #023
      *  JTG 000111 ADDED SP-INSREJECT-FG OPTION 'C', WHICH WILL CAUSE WRITTING
      *             NO OF MONTHS INSURANCE NOT IN VIOLATION, WISCONSIN #1032
      *  MJD 000928 ADDED F2-MEMO OPTION FOR NEW SPR MEMO FILE (SMFILE)
      *  MG  020712 ADDED AN IR-SCAN TO INSURANCE-RATE-CD (#02) DR#0008
      *   CS 041020 ADD CL/SPMEMO WHICH WRITE SPMEMO(SMFILE) WHEN A FIELD IS
      *             CHANGED [WORLD PR#362]
      *  MJD 060824 ADDED INS CHARGEABLE CODE 'D' TO FIELD 7 DISPLAY MESS.
      *                                                         PACESETTER #2771
      *  BAH 090624 ADDED SP-INSEXCEPT 15, COMBINED 2 AND 13, REGWAN PR# 3383
      *  CS 160728 CHANGED SAVEKEY FROM X(8) TO X(9), ORIGINAL-SP-REC TO
      *            ORIGINAL-SP-REC, CHANGED-SP-REC TO CHANGED-SP-REC
      *  CS 170505 ACTIVATE 8 DIGIT DATES
      *  BAH 20191002 DECREASED SP-INSVERTBL TO OCCURS 5 FROM 14.
      *  BAH 2021.0813 ADDED SP-INSREJECT-FG "F" #1510
      **********************************************************
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
           "@(#)MN/SPMANA.CBL S35 04/11/22-12:09:38 >".
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
       COPY "LIBLP/LP01IC.CPY".
       COPY "LIBLP/LP01IC_SQL.CPY".
       COPY "LIBLP/LPWSIC.CPY".
       COPY "LIBLP/LP01IR.CPY".
       COPY "LIBLP/LP01IR_SQL.CPY".
       COPY "LIBLP/LPWSIR.CPY".

       01  ONE               PIC S9.
       01  SUB               PIC 99     COMP-3 VALUE 0.
       01  SUB1              PIC 99     COMP-3 VALUE 0.
       01  S                 PIC 99     COMP-3.
       01  ELE               PIC 99     VALUE 0.
       01  ELE-OVER-20       PIC 99     VALUE 0.
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
       01  DISPLAY1-BUF        VALUE SPACES.
           03  F01             PIC ZZ.
           03  F02             PIC ZZZ.
           03  F03             PIC ZZZ9.99   BLANK ZERO.
           03  F04             PIC Z9.99     BLANK ZERO.
           03  F05             PIC ZZ9.
           03  F06             PIC Z9.9999   BLANK ZERO.
           03  F07             PIC X.
           03  F071            PIC X.
           03  F08             PIC ZZZZZ9    BLANK ZERO.
           03  F081            PIC ZZZZZ9    BLANK ZERO.
           03  F09             PIC ZZZ9.99   BLANK ZERO.
           03  F10             PIC ZZZZZ9    BLANK ZERO.
           03  F11             PIC ZZ9       BLANK ZERO.
           03  F12             PIC X.
           03  F121            PIC X.
           03  F13             PIC XX.
           03  F131            PIC X.
           03  F14             PIC ZZ.
           03  F142            PIC X.
           03  INS-VERIFY      OCCURS 14.
               05  F-MINAGE    PIC ZZ.
               05  F-MAXAGE    PIC ZZ.
               05  F-MAXCOV    PIC ZZZZZ9    BLANK ZERO.
               05  F-MAXTRM    PIC ZZ9       BLANK ZERO.
       01  DISPLAY2-BUF        VALUE SPACES.
           03  INS-RATES       OCCURS 19.
               05  F-INSTRM    PIC ZZ9       BLANK ZERO.
               05  F-INSRTE    PIC Z9.9999   BLANK ZERO.

       01  DISPLAY1-LIST.
           03  FILLER        PIC X(52)  VALUE
           "010 020 030 040 050 060 070 071 080 081 090 100 110 ".
           03  FILLER        PIC X(56)  VALUE
           "120 121 130 131 140 142 150 151 152 153 160 161 162 163 ".
           03  FILLER        PIC X(48)  VALUE
           "170 171 172 173 180 181 182 183 190 191 192 193 ".
           03  FILLER        PIC X(48)  VALUE
           "200 201 202 203 210 211 212 213 220 221 222 223 ".
           03  FILLER        PIC X(48)  VALUE
           "230 231 232 233 240 241 242 243 250 251 252 253 ".
           03  FILLER        PIC X(48)  VALUE
           "260 261 262 263 270 271 272 273 280 281 282 283.".
       01  DISPLAY2-LIST.
           03  FILLER        PIC X(48)  VALUE
           "290 291 300 301 310 311 320 321 330 331 340 341 ".
           03  FILLER        PIC X(48)  VALUE
           "350 351 360 361 370 371 380 381 390 391 400 401 ".
           03  FILLER        PIC X(56)  VALUE
           "410 411 420 421 430 431 440 441 450 451 460 461 470 471.".

      *    **********************************
      *    *   ELEMENT SPECIFICATIONS
      *    **********************************
       01  MAX              PIC 99 COMP-3 VALUE 47.
       01  SPEC-TABLE.
           03  FILLER        PIC X(8)   VALUE "ENNN020 ".
      * 2. SP-INSRATECD
           03  FILLER        PIC X(8)   VALUE "ENNN030 ".
           03  FILLER        PIC X(8)   VALUE "ENNN042 ".
           03  FILLER        PIC X(8)   VALUE "ENNN022 ".
      * 5. SP-INSTERM(6 1)
           03  FILLER        PIC X(8)   VALUE "ENNN030 ".
           03  FILLER        PIC X(8)   VALUE "ENNN024 ".
           03  FILLER        PIC X(8)   VALUE "E  A010 ".
           03  FILLER        PIC X(8)   VALUE "ENNN060 ".
           03  FILLER        PIC X(8)   VALUE "ENNN042 ".
      * 10. SP-CAL-INSMAXBENIF(6)
           03  FILLER        PIC X(8)   VALUE "ENNN060 ".
           03  FILLER        PIC X(8)   VALUE "ENNN030 ".
           03  FILLER        PIC X(8)   VALUE "E  A010 ".
           03  FILLER        PIC X(8)   VALUE "E  A020 ".
           03  FILLER        PIC X(8)   VALUE "ENNN020 ".
      * 15. SP-INSMINAGE(6 1)
           03  FILLER        PIC X(8)   VALUE "ENNN020 ".
           03  FILLER        PIC X(8)   VALUE "ENNN020 ".
           03  FILLER        PIC X(8)   VALUE "ENNN020 ".
           03  FILLER        PIC X(8)   VALUE "ENNN020 ".
           03  FILLER        PIC X(8)   VALUE "ENNN020 ".
      * 20. SP-INSMINAGE(6 6)
           03  FILLER        PIC X(8)   VALUE "ENNN020N".
           03  FILLER        PIC X(8)   VALUE "ENNN020N".
           03  FILLER        PIC X(8)   VALUE "ENNN020N".
           03  FILLER        PIC X(8)   VALUE "ENNN020N".
           03  FILLER        PIC X(8)   VALUE "ENNN020N".
      * 25. SP-INSMINAGE(6 11)
           03  FILLER        PIC X(8)   VALUE "ENNN020N".
           03  FILLER        PIC X(8)   VALUE "ENNN020N".
           03  FILLER        PIC X(8)   VALUE "ENNN020N".
           03  FILLER        PIC X(8)   VALUE "ENNN020N".
           03  FILLER        PIC X(8)   VALUE "ENNN030 ".
      * 30. SP-INSTERM(6 1)
           03  FILLER        PIC X(8)   VALUE "ENNN030 ".
           03  FILLER        PIC X(8)   VALUE "ENNN030 ".
           03  FILLER        PIC X(8)   VALUE "ENNN030 ".
           03  FILLER        PIC X(8)   VALUE "ENNN030 ".
           03  FILLER        PIC X(8)   VALUE "ENNN030 ".
      * 35. SP-INSTERM(6 7)
           03  FILLER        PIC X(8)   VALUE "ENNN030 ".
           03  FILLER        PIC X(8)   VALUE "ENNN030 ".
           03  FILLER        PIC X(8)   VALUE "ENNN030 ".
           03  FILLER        PIC X(8)   VALUE "ENNN030 ".
           03  FILLER        PIC X(8)   VALUE "ENNN030 ".
      * 40. SP-INSTERM(6 12)
           03  FILLER        PIC X(8)   VALUE "ENNN030 ".
           03  FILLER        PIC X(8)   VALUE "ENNN030 ".
           03  FILLER        PIC X(8)   VALUE "ENNN030 ".
           03  FILLER        PIC X(8)   VALUE "ENNN030 ".
           03  FILLER        PIC X(8)   VALUE "ENNN030 ".
      * 45. SP-INSTERM(6 17)
           03  FILLER        PIC X(8)   VALUE "ENNN030 ".
           03  FILLER        PIC X(8)   VALUE "ENNN030 ".
           03  FILLER        PIC X(8)   VALUE "ENNN030 ".
       01  SPEC-TAB       REDEFINES SPEC-TABLE.
           03  SPEC-REC3   OCCURS 47 TIMES.
               05  SPEC      PIC X(7).
               05  CHFG      PIC X.

       COPY "LIBWI/IRSCANW.CPY".
       COPY "LIBWI/CDBSCNW.CPY".
       COPY "LIBWI/SPSCANW.CPY".
       COPY "LIBCL/CLMENUW_EXT.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBWI/MEMOSPW.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBWI/LCSCANW.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBMN/SPMANA_DEF.CPY".
       COPY "LIBMN/SPMANA_WKS.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/ACCESSW.CPY".

       COPY "LIBGB/GETFMW.CPY".
       01 FORM-LINK-AREA    PIC X(2000).

       SCREEN SECTION.
       COPY "LIBMN/SPMANA_SCN.CPY".

       PROCEDURE DIVISION
            USING FORM-PATHNAME FORM-LINK-AREA EXIT-PATHNAME.

      **************************************
       MAIN-PROGRAM SECTION.
      **************************************
           PERFORM SQL-CONNECT.

       COPY "LIBGB/CHKSEC.CPY".
           MOVE " " TO VDU-WIMENU-SELECTION
                       VDU-PGMENU-SELECTION.
           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "SPMANA" TO VDU-FORM-NAME.
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
              MOVE 11 TO FORM-SEL2.
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

      **************************************
       REC-SP-MODULE SECTION.
      **************************************
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

      **************************************
       IR-SCAN SECTION.
      **************************************

           MOVE 0           TO IRSCAN-IR-KEY.
           MOVE "WI/IRSCAN" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH IRSCAN-BUF EXIT-PATHNAME.
           CANCEL FORM-PROGX.

           IF (IRSCAN-STATUS NOT = "00") OR (IRSCAN-IR-KEY =  0)
              MOVE "XX" TO VDUSTATUS
              GO TO EXIT-IR-SCAN.

           MOVE "00" TO VDUSTATUS.

           MOVE "SNNN020020." TO VDU.
           COMPUTE VDUNUM0 = IRSCAN-IR-KEY + 70.
           PERFORM SCREENIO.
           COMPUTE VDUNUM0 = IRSCAN-IR-KEY + 70.

       EXIT-IR-SCAN.
           EXIT.

      **************************************
       CLASS-SCAN SECTION.
      **************************************
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

      **************************************
       ENTER-SCANKEY SECTION.
      **************************************
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
              PERFORM VALIDATE-INS-CODES
              IF IO-FG NOT = 0
                 GO TO MODIFY-ROUTINE
              ELSE
                 MOVE "RWR" TO IO-TYPE
                 GO TO END-ENTRY-MODULE.
      * FORM-NUM1 CHECKS FOR PAGES 1 THRU 9
           IF FORM-KEY1 = " " AND FORM-P1 = "P" AND FORM-SEL1 NUMERIC
              IF FORM-SEL1 < 1
                 GO TO MODIFY-ROUTINE
              ELSE
                 GO TO CHECK-PAGE.
      * FORM-NUM2 CHECKS FOR PAGE 10 THRU 14
           IF FORM-KEY2 = " " AND FORM-P2 = "P" AND FORM-SEL2 NUMERIC
              IF NOT FORM-2-DIGITS OR FORM-SEL2 = 10
                 GO TO MODIFY-ROUTINE
              ELSE
                 GO TO CHECK-PAGE.
           GO TO CHK-CHNO.
       CHECK-PAGE.
           PERFORM VALIDATE-INS-CODES.
           IF IO-FG NOT = 0
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
       TEST-F6.
           IF VDUSTATUS = "1<"
              IF ELE = 13
                 PERFORM CODE-SCAN
                 IF IO-FG = 0
                    GO TO GOTO-FIELD
                 ELSE
                    GO TO ENTER-ELE
              ELSE
              IF ELE = 02
                 PERFORM IR-SCAN.
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
           IF ELE-OVER-20 > 20
              SUBTRACT 20 FROM ELE-OVER-20 GIVING ELE-OVER-20
              GO TO OVER-40.
           GO TO F-21 F-22 F-23 F-24 F-25
                 F-26 F-27 F-28 F-29 F-30
                 F-31 F-32 F-33 F-34 F-35
                 F-36 F-37 F-38 F-39 F-40
                 DEPENDING ON ELE-OVER-20.
           GO TO END-ENTRY-MODULE.
       OVER-40.
           GO TO F-41 F-42 F-43 F-44 F-45
                 F-46 F-47
                 DEPENDING ON ELE-OVER-20.
           GO TO END-ENTRY-MODULE.

      ***************************
      *    SET RECORDS FIELDS
      ***************************
       F-01.
           MOVE VDUNUM0 TO SP-INSCOM(6) IC-CODE.
           PERFORM VALIDATE-INSCOMP.
           IF IO-FG = 9
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       F-02.
           MOVE VDUNUM0 TO SP-INSRATECD(6).
           IF (NOT SP-INSRATECD-VALID(6))
                         OR ( SP-INSRATECD(6) = 5 OR 6)
              MOVE "ENTER:  1, 2, 3, 4, 7, 11 OR (71-99)" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           IF SP-INSRATECD(6) > 70
              SUBTRACT 70 FROM SP-INSRATECD(6) GIVING IR-KEY
              PERFORM GET-IR
              IF IO-FG = 9
                 MOVE "INSURANCE RATE RECORD, NOT ON FILE" TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       F-03.
           MOVE VDUNUM2 TO SP-INSJFAC(6).
           GO TO CREATE-ROUTINE.
       F-04.
           MOVE VDUNUM2 TO SP-INSDISCNT(6).
           GO TO CREATE-ROUTINE.
       F-05.
           MOVE VDUNUM0 TO SP-INSTERM(6 1).
           GO TO CREATE-ROUTINE.
       F-06.
           MOVE VDUNUM4 TO SP-INSRATE(6 1).
           GO TO CREATE-ROUTINE.
       F-07.
           MOVE VDUBUF TO SP-INSCHGABL(6).
           IF NOT SP-INSCHGABL-VALID(6)
              STRING ":'P' 'N' 'C' 'X' 'M' 'R' 'A' 'I' "
                     "'H' 'J' 'V' '3' 'O' 'F' 'D' 'E'"
                     DELIMITED BY SIZE INTO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
       F-071.
           MOVE "E  A010071." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF VDUSTATUS = "1D"
              MOVE SP-INSCHGABL-FG(6) TO VDUBUF
              MOVE "00" TO VDUSTATUS.
           IF VDUSTATUS NOT = "00"
              GO TO F-071.
           MOVE VDUBUF TO SP-INSCHGABL-FG(6).
           IF NOT SP-INSCHGABL-FG-VALID(6)
              MOVE "ENTER: SPACE OR 'D'" TO MESS
              PERFORM SEND-MESS
              GO TO F-071.
           GO TO CREATE-ROUTINE.
       F-08.
           MOVE VDUNUM0 TO SP-INSMINCHGABL(6).
       F-081.
           MOVE "ENNN060081." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF VDUSTATUS NOT = "00"
              GO TO F-081.
           MOVE VDUNUM0 TO SP-INSMAXCHGABL(6).
           IF SP-INSMAXCHGABL(6) < SP-INSMINCHGABL(6)
              GO TO F-081.
           GO TO CREATE-ROUTINE.
       F-09.
           MOVE VDUNUM2 TO SP-INSMINPREM(6).
           GO TO CREATE-ROUTINE.
       F-10.
           MOVE VDUNUM0 TO SP-INSMAXBENIF(6).
           IF SP-INSMAXBENIF(6) > 0
              MOVE "NOT AVAILABLE" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       F-11.
           MOVE VDUNUM0 TO SP-INSMAXTERM(6).
           GO TO CREATE-ROUTINE.
       F-12.
           MOVE VDUBUF TO SP-INSCHGBEN(6).
           IF NOT SP-INSCHGBEN-VALID(6)
              MOVE "ENTER: 'B' OR 'C'" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
       F-121.
           MOVE "E NA010121." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF VDUSTATUS = "1D"
              MOVE SP-INSREJECT-FG(6) TO VDUBUF
              MOVE "00" TO VDUSTATUS.
           IF VDUSTATUS NOT = "00"
              GO TO F-121.
           MOVE VDUBUF TO SP-INSREJECT-FG(6).
           IF NOT SP-INSREJECT-FG-VALID(6)
              MOVE "ENTER: SPACE, 'A', 'B', 'C', 'D', 'E' OR 'F'"
                                                         TO MESS
              PERFORM SEND-MESS
              GO TO F-121.
           GO TO CREATE-ROUTINE.
       F-13.
           IF VDUBUF = "0" OR "00"
              GO TO ENTER-ELE.
           MOVE VDUBUF TO SP-INSFEECD(6).
           IF VDUBUF = " "
              MOVE "S  A010131." TO VDU
              MOVE " " TO SP-INSFEE-FRMLA(6) VDUBUF
              PERFORM SCREENIO
              GO TO CREATE-ROUTINE.
           IF SP-INSFEECD(7) NOT = " "
              IF SP-INSFEECD(6) NOT = SP-INSFEECD(7)
                 MOVE "ALL PER. PROPERTY FEE CODES MUST MATCH"
                                                         TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-ELE.
       F-131.
           MOVE "E  A010131." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF VDUSTATUS NOT = "00"
              GO TO F-131.
           IF VDUBUF NOT = "A"
              MOVE "ENTER 'A' ONLY" TO MESS
              PERFORM SEND-MESS
              GO TO F-131.
           MOVE VDUBUF TO SP-INSFEE-FRMLA(6).
           GO TO CREATE-ROUTINE.
       F-14.
           MOVE VDUNUM0 TO SP-INSEXCEPT(6).
           IF NOT SP-INSEXCEPT-VALID(6) OR SP-INSEXCEPT(6) = 13
              MOVE "ENTER: 0 THRU 15, NOT 13, ONLY" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
       F-142.
           MOVE "E  A010142." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF VDUSTATUS NOT = "00"
              GO TO F-142.
           IF NOT (VDUBUF = " " OR "Y")
              MOVE "ENTER ' ' OR 'Y' ONLY" TO MESS
              PERFORM SEND-MESS
              GO TO F-142.
           MOVE VDUBUF TO SP-INSFINCHG(6).
           GO TO CREATE-ROUTINE.
       F-15.
           PERFORM ENTER-VERIFY-TABLE.
           GO TO CREATE-ROUTINE.
       F-16.
           PERFORM ENTER-VERIFY-TABLE.
           GO TO CREATE-ROUTINE.
       F-17.
           PERFORM ENTER-VERIFY-TABLE.
           GO TO CREATE-ROUTINE.
       F-18.
           PERFORM ENTER-VERIFY-TABLE.
           GO TO CREATE-ROUTINE.
       F-19.
           PERFORM ENTER-VERIFY-TABLE.
           GO TO CREATE-ROUTINE.
       F-20.
           GO TO CREATE-ROUTINE.
       F-21.
           GO TO CREATE-ROUTINE.
       F-22.
           GO TO CREATE-ROUTINE.
       F-23.
           GO TO CREATE-ROUTINE.
       F-24.
           GO TO CREATE-ROUTINE.
       F-25.
           GO TO CREATE-ROUTINE.
       F-26.
           GO TO CREATE-ROUTINE.
       F-27.
           GO TO CREATE-ROUTINE.
       F-28.
           GO TO CREATE-ROUTINE.
       F-29.
           PERFORM ENTER-RATE-TABLE.
           GO TO CREATE-ROUTINE.
       F-30.
           PERFORM ENTER-RATE-TABLE.
           GO TO CREATE-ROUTINE.
       F-31.
           PERFORM ENTER-RATE-TABLE.
           GO TO CREATE-ROUTINE.
       F-32.
           PERFORM ENTER-RATE-TABLE.
           GO TO CREATE-ROUTINE.
       F-33.
           PERFORM ENTER-RATE-TABLE.
           GO TO CREATE-ROUTINE.
       F-34.
           PERFORM ENTER-RATE-TABLE.
           GO TO CREATE-ROUTINE.
       F-35.
           PERFORM ENTER-RATE-TABLE.
           GO TO CREATE-ROUTINE.
       F-36.
           PERFORM ENTER-RATE-TABLE.
           GO TO CREATE-ROUTINE.
       F-37.
           PERFORM ENTER-RATE-TABLE.
           GO TO CREATE-ROUTINE.
       F-38.
           PERFORM ENTER-RATE-TABLE.
           GO TO CREATE-ROUTINE.
       F-39.
           PERFORM ENTER-RATE-TABLE.
           GO TO CREATE-ROUTINE.
       F-40.
           PERFORM ENTER-RATE-TABLE.
           GO TO CREATE-ROUTINE.
       F-41.
           PERFORM ENTER-RATE-TABLE.
           GO TO CREATE-ROUTINE.
       F-42.
           PERFORM ENTER-RATE-TABLE.
           GO TO CREATE-ROUTINE.
       F-43.
           PERFORM ENTER-RATE-TABLE.
           GO TO CREATE-ROUTINE.
       F-44.
           PERFORM ENTER-RATE-TABLE.
           GO TO CREATE-ROUTINE.
       F-45.
           PERFORM ENTER-RATE-TABLE.
           GO TO CREATE-ROUTINE.
       F-46.
           PERFORM ENTER-RATE-TABLE.
           GO TO CREATE-ROUTINE.
       F-47.
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

           SUBTRACT 27 FROM ELE GIVING S.
           MOVE VDUNUM0 TO SP-INSTERM(6 S).
           IF VDUNUM0 = 0
              PERFORM CLEAR-RATE-TABLE
              GO TO ENTER-RATE-TABLE-EXIT.
       ENTER-AMOUNT.
           MOVE "ENNN024  1." TO VDU.
           MOVE ELE TO VDUROW.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              SUBTRACT 1 FROM ELE
              GO TO ENTER-RATE-TABLE-EXIT.
           IF VDUSTATUS = "1D"
              MOVE SP-INSRATE(6 S) TO VDUNUM4
              MOVE "00" TO VDUSTATUS.
           IF VDUSTATUS NOT = "00"
              GO TO ENTER-AMOUNT.
           MOVE VDUNUM4 TO SP-INSRATE(6 S).
       ENTER-RATE-TABLE-EXIT.
           EXIT.

      ************************************************
       CLEAR-RATE-TABLE SECTION.
      ************************************************

      * CLEAR RATE TABLE FROM S TO END WHEN 0 IS ENTERED

       CLEAR-RATES.
           MOVE "S  A000  0." TO VDU.
           MOVE ELE TO VDUROW.
           PERFORM SCREENIO.
           MOVE 1 TO VDUCOL.
           PERFORM SCREENIO.
           MOVE 0 TO SP-INSTERM(6 S) SP-INSRATE(6 S).
           IF S < 20
              ADD 1 TO S ELE
              GO TO CLEAR-RATES.
       CLEAR-RATE-TABLE-EXIT.
           EXIT.

      ************************************************
       ENTER-VERIFY-TABLE SECTION.
      ************************************************

      * ADJUST ELE TO SUBSCRIPT TABLE

           SUBTRACT 14 FROM ELE GIVING S.
           MOVE VDUNUM0 TO SP-INSMINAGE(6 S).
           IF VDUNUM0 = 0
              PERFORM CLEAR-VERIFY-TABLE
              GO TO ENTER-VERIFY-TABLE-EXIT.
       ENTER-MAXAGE.
           MOVE "ENNN020  1." TO VDU.
           MOVE ELE TO VDUROW.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              SUBTRACT 1 FROM ELE
              GO TO ENTER-VERIFY-TABLE-EXIT.
           IF VDUSTATUS = "1D"
              MOVE SP-INSMAXAGE(6 S) TO VDUNUM0
              MOVE "00" TO VDUSTATUS.
           IF VDUSTATUS NOT = "00"
              GO TO ENTER-MAXAGE.
           MOVE VDUNUM0 TO SP-INSMAXAGE(6 S).
       ENTER-MAXCOV.
           MOVE "ENNN060  2." TO VDU.
           MOVE ELE TO VDUROW.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-MAXAGE.
           IF VDUSTATUS = "1D"
              MOVE SP-INSMAXCOV(6 S) TO VDUNUM0
              MOVE "00" TO VDUSTATUS.
           IF VDUSTATUS NOT = "00"
              GO TO ENTER-MAXCOV.
           MOVE VDUNUM0 TO SP-INSMAXCOV(6 S).
       ENTER-MAXTRM.
           MOVE "ENNN030  3." TO VDU.
           MOVE ELE TO VDUROW.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-MAXCOV.
           IF VDUSTATUS = "1D"
              MOVE SP-INSMAXTRM(6 S) TO VDUNUM0
              MOVE "00" TO VDUSTATUS.
           IF VDUSTATUS NOT = "00"
              GO TO ENTER-MAXTRM.
           MOVE VDUNUM0 TO SP-INSMAXTRM(6 S).
       ENTER-VERIFY-TABLE-EXIT.
           EXIT.

      ************************************************
       CLEAR-VERIFY-TABLE SECTION.
      ************************************************

      * CLEAR VERIFY TABLE FROM S TO END WHEN 0 IS ENTERED

       CLEAR-VERIFYS.
           MOVE "S  A000  0." TO VDU.
           MOVE ELE TO VDUROW.
           PERFORM SCREENIO.
           MOVE 1 TO VDUCOL.
           PERFORM SCREENIO.
           MOVE 2 TO VDUCOL.
           PERFORM SCREENIO.
           MOVE 3 TO VDUCOL.
           PERFORM SCREENIO.
           MOVE 0 TO SP-INSMINAGE(6 S) SP-INSMAXAGE(6 S)
                     SP-INSMAXCOV(6 S) SP-INSMAXTRM(6 S).
           IF S < 5
              ADD 1 TO S ELE
              GO TO CLEAR-VERIFYS.
       CLEAR-EDIT-TABLE-EXIT.
           EXIT.

      ************************************************
       CODE-SCAN SECTION.
      ************************************************
           MOVE "GC"                TO CDBSCN-TYPE.
           MOVE EXT-CONAME-USER-LOC TO CDBSCN-BRANCH.
           MOVE SPACES              TO CDBSCN-CODE.

           MOVE "WI/CDBSCN" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH CDBSCN-BUF EXIT-PATHNAME.
           CANCEL FORM-PROGX.
           IF ( CDBSCN-STATUS NOT = "00") OR (CDBSCN-CODE = SPACES) 
              MOVE 9 TO IO-FG
              GO TO EXIT-CODE-SCAN.

           MOVE "S  A020130." TO VDU.
           MOVE ELE TO VDUROW.
           MOVE CDBSCN-CODE TO VDUBUF.
           PERFORM SCREENIO.

           MOVE CDBSCN-CODE TO VDUBUF.
           MOVE 0 TO IO-FG.

       EXIT-CODE-SCAN.
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
           MOVE SPACES TO DISPLAY1-BUF DISPLAY2-BUF.
           MOVE "S  8000EFFDATE." TO VDU.
           MOVE SP-EFFDATE TO VDU-DATE-YYYYMMDD.
           PERFORM SCREENIO.
           MOVE "S  8000EXPDATE." TO VDU.
           MOVE SP-EXPDATE TO VDU-DATE-YYYYMMDD.
           PERFORM SCREENIO.
           PERFORM DISPLAY-SUBCLASS-DESC.

           MOVE SP-INSCOM(6) TO F01 IC-CODE.
           MOVE SP-INSRATECD(6) TO F02.
           MOVE SP-INSJFAC(6) TO F03.
           MOVE SP-INSDISCNT(6) TO F04.
           MOVE SP-INSTERM(6 1) TO F05.
           MOVE SP-INSRATE(6 1) TO F06.
           MOVE SP-INSCHGABL(6) TO F07.
           MOVE SP-INSCHGABL-FG(6) TO F071.
           MOVE SP-INSMINCHGABL(6) TO F08.
           MOVE SP-INSMAXCHGABL(6) TO F081.
           MOVE SP-INSMINPREM(6) TO F09.
           MOVE SP-INSMAXBENIF(6) TO F10.
           MOVE SP-INSMAXTERM(6) TO F11.
           MOVE SP-INSCHGBEN(6) TO F12.
           MOVE SP-INSREJECT-FG(6) TO F121.
           MOVE SP-INSFEECD(6) TO F13.
           MOVE SP-INSFEE-FRMLA(6) TO F131.
           MOVE SP-INSEXCEPT(6) TO F14.
           MOVE SP-INSFINCHG(6) TO F142.

           MOVE 0 TO SUB.
       NEXT-VERIFY.
           ADD 1 TO SUB.
           MOVE SP-INSMINAGE(6 SUB) TO F-MINAGE(SUB).
           MOVE SP-INSMAXAGE(6 SUB) TO F-MAXAGE(SUB).
           MOVE SP-INSMAXCOV(6 SUB) TO F-MAXCOV(SUB).
           MOVE SP-INSMAXTRM(6 SUB) TO F-MAXTRM(SUB).
           IF SUB < 5
              GO TO NEXT-VERIFY.

           MOVE 1 TO SUB.
           MOVE 0 TO SUB1.
       NEXT-RATE.
           ADD 1 TO SUB SUB1.
           MOVE SP-INSTERM(6 SUB) TO F-INSTRM(SUB1).
           MOVE SP-INSRATE(6 SUB) TO F-INSRTE(SUB1).
           IF SUB < 20
              GO TO NEXT-RATE.

      *    PERFORM FORM-IT.
           MOVE DISPLAY1-BUF TO WR-BUF.
           MOVE DISPLAY1-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
           IF STAT NOT = "00"
              GO TO IO-ERROR.
           MOVE DISPLAY2-BUF TO WR-BUF.
           MOVE DISPLAY2-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
           IF STAT NOT = "00"
              GO TO IO-ERROR.
           PERFORM VALIDATE-INSCOMP.

      ************************************************
       VALIDATE-INSCOMP SECTION.
      ************************************************
           IF IC-CODE = 0
              MOVE 0 TO IO-FG
              MOVE " " TO IC-NAME
              GO TO VALIDATE-INSCOMP-DISP.

           PERFORM OPEN-IC1-FILE.
           PERFORM READ-IC1-FILE.
           PERFORM CLOSE-IC1-FILE.
           IF IO-FG NOT = 0
              MOVE "** INVALID **" TO IC-NAME.
       VALIDATE-INSCOMP-DISP.
           MOVE "S  A000011." TO VDU.
           MOVE IC-NAME TO VDUBUF.
           PERFORM SCREENIO.
       VALIDATE-INSCOMP-EXIT.
           EXIT.

       COPY "LIBLP/LPTBNO.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/DATER.CPY".

      ************************************************
       FORM-RESET SECTION.
      ************************************************
           MOVE EXT-CONAME-CONAME TO SPMANA-CONAME.
           MOVE EXT-CONAME-SYSDATE TO SPMANA-SYSDATE.
           DISPLAY SPACES UPON SPMAIN-WINDOW-HANDLE
                               LINE 0 COL 0 ERASE SCREEN.
           DISPLAY SPMANA-SCREEN UPON SPMAIN-WINDOW-HANDLE.
           MOVE SPMANA-FORM-FIELDS TO WR-BUF.
           MOVE SPMANA-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      ************************************************
       SETUP-LINK-INFO SECTION.
      ************************************************
           MOVE SPMANA-HELP--FILE TO HELP-LINK-FILE.
           MOVE SPMANA-HELP--DIR TO HELP-LINK-DIR.

      ************************************************
       VDU-CONVERT-FIELD SECTION.
      ************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBMN/SPMANA_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ************************************************
       MISC-PARAGRAPHS SECTION.
      ************************************************
       COPY "LIBGB/ACCESS.CPY".
       GET-IR.
           PERFORM OPEN-IR-FILE.
           PERFORM READ-IR-FILE.
           PERFORM CLOSE-IR-FILE.
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
           MOVE "S  A160LCDESC." TO VDU.
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
       VALIDATE-INS-CODES.
           MOVE 0 TO IO-FG.

      ************************************************
       IO-ROUTINES SECTION.
      ************************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-ST1-FILE.
           PERFORM CLOSE-SP1-FILE.
           PERFORM CLOSE-LC1-FILE.
           PERFORM CLOSE-IR-FILE.
           PERFORM CLOSE-IC1-FILE.
       COPY "LIBLP/LPSTGS_SQL.CPY".
       COPY "LIBLP/LPSPGS_SQL.CPY".
       COPY "LIBLP/LPLCGS_SQL.CPY".
       COPY "LIBLP/LPIRGS_SQL.CPY".
       COPY "LIBLP/LPICGS_SQL.CPY".
       COPY "LIBLP/LPSP1IN.CPY".
       COPY "LIBLP/LPST1RN.CPY".
       COPY "LIBLP/LPLC1RN.CPY".
       COPY "LIBLP/LPIC1RN.CPY".
       COPY "LIBLP/LPIRRN.CPY".
       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO ERRCD.

      ************************************************
       END-ROUTINE SECTION.
      ************************************************
           PERFORM CLOSE-FILES.
       EXIT-PROG.
       STOP-RUN.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY SPMANA-SCREEN.
           MOVE FORM-PATH TO FORM-PATHNAME.
           MOVE SPMAIN-LINK TO FORM-LINK-AREA.
           EXIT PROGRAM.
