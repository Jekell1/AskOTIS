       IDENTIFICATION DIVISION.
       PROGRAM-ID.      SPMANN.
       AUTHOR.          KEC.
       DATE-WRITTEN.    092598.
      ******************************************************************
      *
      *   PROGRAM NAME:  MN/SPMANN.C
      *
      *   DESCRIPTION :  STATE PARAMETER MAINTENANCE - PAGE 23.
      *
      *-----------------------------------------------------------------
      * REV:
      * KEC 010608 {PR#01478} REGIONAL ACCEPTANCE [KEVIN MULLEN]
      *            ***NEW*** FOR REPO FORM NUMBERS (3 LEVELS).
      *
      *   CS 041020 ADD CL/SPMEMO WHICH WRITES SPR MEMO(SMFILE) WHEN FIELD IS
      *             CHANGED [WORLD PR#362]
      * BAH 090527 ADDED SP-AUTO-SEND-TRCD,FMNO FOR REGACC PR# 245
      *  CS 160803 SAVEKEY FROM X(8) TO X(9), ORIGINAL-SP-REC TO
      *            ORIGINAL-SP-REC, CHANGED-SP-REC TO CHANGED-SP-REC
      * BAH 161010 ADDED SP-PERCENT-REGPAY-AVAIL FOR WORLD PR# 1194
      *            NEW POLICY WILL REQUIRE THAT THE CUSTOMER HAVE AT LEAST
      *            1/2 OF THE REGPAYAMT AVAILABLE IN ORDER TO DISPLAY CASH
      *            AVAILABLE. WILL ENTER 50 IN THIS NEW PERCENT.
      *  CS 170505 ACTIVATE 8 DIGIT DATES
      * BAH 171204 ADDED SP-NON-LOAN-FLAG AND SP-SKIP-TRW FOR TAX LOANS
      *                                                  WORLD PR#1288
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

       DATA DIVISION.
       FILE SECTION.

      ******************************************************************
      *
      *    W O R K I N G - S T O R A G E - S E C T I O N
      *
      ******************************************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)MN/SPMANN.CBL S34 06/08/21-11:00:07 >".

      ******************************************************************
      *
      *    F I L E - P A T H - W O R K E R S
      *
      ******************************************************************
       COPY "LIBLP/LP01FD.CPY".
       COPY "LIBLP/LP01FD_SQL.CPY".
       COPY "LIBLP/LPWSFD.CPY".
       COPY "LIBLP/LP01LC.CPY".
       COPY "LIBLP/LP01LC_SQL.CPY".
       COPY "LIBLP/LPWSLC.CPY".
       COPY "LIBLP/LP01SP.CPY".
       COPY "LIBLP/LP01SPA.CPY".
       COPY "LIBLP/LP01SPB.CPY".
       COPY "LIBLP/LP01SPC.CPY".
       COPY "LIBLP/LP01SP_SQL.CPY".
       COPY "LIBLP/LPWSSP.CPY".
       COPY "LIBLP/LP01ST.CPY".
       COPY "LIBLP/LP01ST_SQL.CPY".
       COPY "LIBLP/LPWSST.CPY".

      ******************************************************************
      *
      *    M I S C E L L A N E O U S - W O R K E R S
      *
      ******************************************************************
       01  CURRENT-VDUCOL              PIC 9(01)     VALUE ZEROES.
       01  FIRST-SCAN                  PIC 9(01)     VALUE ZEROES.

       01  IO-TYPE                     PIC X(03)     VALUE SPACES.
       01  NEW-FLAG                    PIC 9(01)     VALUE ZEROES.
       01  ONE                         PIC S9(1).
       01  ONE-F01                     PIC S9(1).

       01  CHANGED-SP-REC             PIC X(SP-SIZE) VALUE SPACES.
       01  ORIGINAL-SP-REC            PIC X(SP-SIZE) VALUE SPACES.

       01  SUB                        PIC 9(02)     COMP-3 VALUE ZERO.

      *-----------------------------------------------------------------
       01  WS-F01-VDU                                VALUE SPACES.
           03  WS-F01-VDUPROCESS.
               05  WS-F01-VDUACTION                  PIC X(01).
               05  WS-F01-VDUPUNC                    PIC X(01).
               05  WS-F01-VDURESET                   PIC X(01).
               05  WS-F01-VDUCLASS                   PIC X(01).
               05  WS-F01-VDULENGTH.
                   07  WS-F01-VDUNUM                 PIC 9(02).
                   07  WS-F01-VDUDEC                 PIC 9(01).
               05  WS-F01-VDUFIELD.
                   07  WS-F01-VDUROW                 PIC 9(02).
                   07  WS-F01-VDUCOL-ALP.
                       09  WS-F01-VDUCOL             PIC 9(01).
                   07  WS-F01-VDUATTR.
                       09  WS-F01-VDUDESC-D          PIC X(01).
                       09  WS-F01-VDUDECIMAL         PIC X(01).
                       09  FILLER                    PIC X(08).

       01  SPMAIN-LINK VALUE SPACES.
           03  FORM-NAME     PIC X(20).
           03  SAVEKEY       PIC X(9).
           03  NEW-REC       PIC X.
           03  ERRCD         PIC X.
           03  FORMSEL       PIC 99.
           03  SPMAIN-WINDOW-HANDLE PIC X(10).

      ******************************************************************
      *
      *    D I S P L A Y - B U F F E R
      *
      ******************************************************************
       01  DISPLAY-BUF                 VALUE SPACES.
           03  F01                     PIC X(2).
           03  F011                    PIC 9(03)     OCCURS 4 TIMES.
           03  F011D                   PIC X(22)     OCCURS 4 TIMES.

           03  F02                     PIC X(2).
           03  F021                    PIC 9(3)      BLANK ZERO
                                                     OCCURS 4 TIMES.
           03  F021D                   PIC X(22)     OCCURS 4 TIMES.

           03  F03                     PIC X(2).
           03  F031                    PIC 9(3)      BLANK ZERO
                                                     OCCURS 4 TIMES.
           03  F031D                   PIC X(22)     OCCURS 4 TIMES.
           03  F04                     PIC 99.
           03  F05                     PIC X.
           03  F06                     PIC X.

      *-----------------------------------------------------------------
       01  DISPLAY-LIST.
           03  FILLER PIC X(20) VALUE "010 011 012 013 014 ".
           03  FILLER PIC X(20) VALUE "011D 012D 013D 014D ".
           03  FILLER PIC X(20) VALUE "020 021 022 023 024 ".
           03  FILLER PIC X(20) VALUE "021D 022D 023D 024D ".
           03  FILLER PIC X(20) VALUE "030 031 032 033 034 ".
           03  FILLER PIC X(24) VALUE "031D 032D 033D 034D 040 ".
           03  FILLER PIC X(8)  VALUE "050 060.".


      *========================================================[ TTF01 ]
       77  TTF01-ELE                   PIC 9(01)     VALUE ZEROES.
           88  TTF01-ELE-VALID                       VALUE 01 THRU 03.

      *-----------------------------------------------------------------
       01  TOGGLE-TRCDS-F01-BUF.
           03  TTF01-CURRENT-TRCD      PIC X(02).
           03  TTF01-CURRENT-FMNO      PIC 9(03)     OCCURS 4 TIMES.
           03  TTF01-CURRENT-FMNO-DESC PIC X(22)     OCCURS 4 TIMES.
           03  TTF01-ALL-TRCD-BUF                    OCCURS 3 TIMES.
               05  FILLER              PIC X(01).
               05  TTF01-ALL-TRCD      PIC X(02).
               05  FILLER              PIC X(01).

       01  TOGGLE-TRCDS-F01-LIST.
           03  FILLER        PIC X(20) VALUE "010 011 012 013 014 ".
           03  FILLER        PIC X(20) VALUE "011D 012D 013D 014D ".
           03  FILLER        PIC X(07) VALUE "TRCD-01".
           03  TTF01-01-ATTR PIC X(04) VALUE ":FR ".
           03  FILLER        PIC X(07) VALUE "TRCD-02".
           03  TTF01-02-ATTR PIC X(04) VALUE ":R  ".
           03  FILLER        PIC X(07) VALUE "TRCD-03".
           03  TTF01-03-ATTR PIC X(04) VALUE ":R  ".
           03  FILLER        PIC X(01) VALUE ".".

      ******************************************************************
      *
      *    E L E M E N T - S P E C I F I C A T I O N S
      *
      ******************************************************************
       78  MAX                                       VALUE 06.
       77  ELE                         PIC 9(02)     VALUE ZEROES.

      *-----------------------------------------------------------------
       01  SPEC-TABLE.
           03  FILLER PIC X(08) VALUE "E  A020 ".
           03  FILLER PIC X(08) VALUE "E  A010 ".
           03  FILLER PIC X(08) VALUE "E  A010 ".
           03  FILLER PIC X(08) VALUE "ENNN020 ".
      * SP-NON-LOAN-FLAG
           03  FILLER PIC X(08) VALUE "E  A010 ".
      * SP-SKIP-TRW
           03  FILLER PIC X(08) VALUE "E  A010 ".

      *-----------------------------------------------------------------
       01  SPEC-TAB                    REDEFINES SPEC-TABLE.
           03  SPEC-REC                OCCURS MAX TIMES.
               05  SPEC                PIC X(07).
               05  CHFG                PIC X(01).

      ******************************************************************
      *
      *    C O P Y - L I B R A R Y - W O R K E R S
      *
      ******************************************************************
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBWI/LCSCANW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBWI/MEMOSPW.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBLP/SPMANSEL.CPY".
       COPY "LIBWI/SPSCANW.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBMN/SPMANN_DEF.CPY".
       COPY "LIBMN/SPMANN_WKS.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/ACCESSW.CPY".

       COPY "LIBGB/GETFMW.CPY".
       01 FORM-LINK-AREA    PIC X(2000).

       SCREEN SECTION.
       COPY "LIBMN/SPMANN_SCN.CPY".

       PROCEDURE DIVISION
            USING FORM-PATHNAME FORM-LINK-AREA EXIT-PATHNAME.

      ******************************************************************
      *
      *    M A I N - P R O G R A M
      *
      ******************************************************************
       MAIN-PROGRAM SECTION.
           PERFORM SQL-CONNECT.

       COPY "LIBGB/CHKSEC.CPY".
           MOVE SPACES TO VDU-WIMENU-SELECTION
                          VDU-PGMENU-SELECTION.
           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "SPMANN" TO VDU-FORM-NAME.
           MOVE FORM-LINK-AREA TO SPMAIN-LINK.

       MAIN-MODULE.

           PERFORM FORM-RESET.
           PERFORM ENTER-PRIMARY-KEY.
           IF ( IO-TYPE = "END" )
              MOVE "X" TO ERRCD
              GO TO EXIT-MAIN-PROGRAM.

           IF ( IO-TYPE = "CLR" )
              GO TO MAIN-MODULE.

           PERFORM ENTRY-MODULE.
           IF ( IO-TYPE = "RWR" )
              PERFORM WRITE-MODULE.

           IF ( NEW-REC = "Y" )
              MOVE "N"    TO NEW-REC
              MOVE "P"    TO FORM-P1
              MOVE 1      TO FORM-SEL1
              MOVE SPACES TO SAVEKEY.

           IF ( FORM-SEL1 NOT = ZEROES ) AND
              ( FORM-P1       = "P"    )
              MOVE SP1-KEY   TO SAVEKEY
              MOVE FORM-SEL1 TO FORMSEL
              GO TO EXIT-MAIN-PROGRAM.

           IF ( FORM-SEL2 NOT = ZEROES ) AND
              ( FORM-P2       = "P"    )
              MOVE SP1-KEY   TO SAVEKEY
              MOVE FORM-SEL2 TO FORMSEL
              GO TO EXIT-MAIN-PROGRAM.

           GO TO MAIN-MODULE.

       EXIT-MAIN-PROGRAM.
           GO TO END-PROGRAM.

      ******************************************************************
       ENTER-PRIMARY-KEY SECTION.
      ******************************************************************
       BEGIN-ENTER-PRIMARY-KEY.

           MOVE SPACES TO IO-TYPE.
           MOVE 1      TO TTF01-ELE.

           IF ( SAVEKEY NOT = SPACES )
              MOVE SAVEKEY TO SP1-KEY
              MOVE SPACES  TO SAVEKEY
              PERFORM DISPLAY-SCAN-KEY
              GO TO READ-SPFILE.

           MOVE "SPCLASS SUBCLASS LCDESC CLASS STDESC SPNO." TO WR-LIST.
           MOVE SPACES TO WR-BUF.
            PERFORM CALL-WRFORM.
           MOVE ZEROES TO NEW-FLAG.

           MOVE "E  A020SPNO." TO VDU.
           PERFORM SCREENIO.

           IF ( F6-KEY ) PERFORM ENTER-SCANKEY
                         GO TO EXIT-ENTER-PRIMARY-KEY.

           IF ( F7-KEY ) MOVE "END" TO IO-TYPE
                         GO TO EXIT-ENTER-PRIMARY-KEY.

           IF NOT ( ENTER-KEY ) GO TO BEGIN-ENTER-PRIMARY-KEY.

           MOVE VDUBUF TO SP-ORGST.

           PERFORM VERIFY-STATE-CODE.

           IF ( IO-BAD )
              GO TO BEGIN-ENTER-PRIMARY-KEY.

       ENTER-SP-CLASS.

           MOVE "LCDESC." TO WR-LIST.
           MOVE SPACES TO WR-BUF.
            PERFORM CALL-WRFORM.

           MOVE "EN N020CLASS." TO VDU.
           PERFORM SCREENIO.

           IF ( UP-KEY ) GO TO BEGIN-ENTER-PRIMARY-KEY.
           IF ( F1-KEY ) GO TO BEGIN-ENTER-PRIMARY-KEY.

           IF ( F5-KEY )
              PERFORM SCAN-LC-FILE
              IF ( IO-BAD )
                 GO TO ENTER-SP-CLASS
              ELSE
                 MOVE "SN N020CLASS." TO VDU
                 MOVE LCSCAN-KEY      TO VDUNUM0
                 PERFORM SCREENIO
                 MOVE LCSCAN-KEY      TO VDUNUM0.

           IF ( F6-KEY )
              MOVE SP-ORGST TO SPSCAN-ORGST
              MOVE ZEROES   TO SPSCAN-SPRCLASS
              MOVE ZEROES   TO SPSCAN-SUBCLASS
              MOVE ZEROES   TO SPSCAN-LAWCODE
              PERFORM SCAN-SP-FILE

              IF ( IO-BAD )
                 GO TO ENTER-SP-CLASS
              ELSE
                 MOVE "SNNN020CLASS."    TO VDU
                 MOVE SPSCAN-SP-SPRCLASS TO VDUNUM0 SP-SPRCLASS
                 PERFORM SCREENIO

                 MOVE "SNNN020SUBCLASS." TO VDU
                 MOVE SPSCAN-SP-SUBCLASS TO VDUNUM0 SP-SUBCLASS
                 PERFORM SCREENIO

                 MOVE "SNNN020SPCLASS." TO VDU
                 MOVE SPSCAN-SP-LAWCODE TO VDUNUM0 SP-LAWCODE
                 PERFORM SCREENIO

                 GO TO READ-SPFILE.

           IF NOT ( ENTER-KEY ) GO TO ENTER-SP-CLASS.

           MOVE VDUNUM0 TO SP-SPRCLASS.
           PERFORM VERIFY-LOAN-CLASS.

           IF ( IO-BAD )
              GO TO ENTER-SP-CLASS.

       ENTER-SP-SUBCLASS.

           MOVE "EN N020SUBCLASS." TO VDU.
           PERFORM SCREENIO.

           IF ( UP-KEY ) GO TO ENTER-SP-CLASS.
           IF ( F1-KEY ) GO TO BEGIN-ENTER-PRIMARY-KEY.

           IF ( F6-KEY )
              MOVE SP-ORGST    TO SPSCAN-ORGST
              MOVE SP-SPRCLASS TO SPSCAN-SPRCLASS
              MOVE ZEROES      TO SPSCAN-SUBCLASS
              MOVE ZEROES      TO SPSCAN-LAWCODE
              PERFORM SCAN-SP-FILE

              IF ( IO-BAD )
                 GO TO ENTER-SP-SUBCLASS
              ELSE
                 MOVE "SNNN020SUBCLASS." TO VDU
                 MOVE SPSCAN-SP-SUBCLASS TO VDUNUM0 SP-SUBCLASS
                 PERFORM SCREENIO

                 MOVE "SNNN020SPCLASS." TO VDU
                 MOVE SPSCAN-SP-LAWCODE TO VDUNUM0 SP-LAWCODE
                 PERFORM SCREENIO

                 GO TO READ-SPFILE.

           IF NOT ( ENTER-KEY ) GO TO ENTER-SP-SUBCLASS.

           MOVE VDUNUM0 TO SP-SUBCLASS.

       ENTER-SP-LAWCODE.

           MOVE "EN N020SPCLASS." TO VDU.
           PERFORM SCREENIO.

           IF ( UP-KEY ) GO TO ENTER-SP-SUBCLASS.
           IF ( F1-KEY ) GO TO BEGIN-ENTER-PRIMARY-KEY.

           IF ( F6-KEY )
              MOVE SP-ORGST    TO SPSCAN-ORGST
              MOVE SP-SPRCLASS TO SPSCAN-SPRCLASS
              ADD 1 SP-SUBCLASS GIVING SPSCAN-SUBCLASS
              MOVE ZEROES      TO SPSCAN-LAWCODE
              PERFORM SCAN-SP-FILE

              IF ( IO-BAD )
                 GO TO ENTER-SP-LAWCODE
              ELSE
                 MOVE "SNNN020SPCLASS." TO VDU
                 MOVE SPSCAN-SP-LAWCODE TO VDUNUM0 SP-LAWCODE
                 PERFORM SCREENIO

                 GO TO READ-SPFILE.

           IF NOT ( ENTER-KEY ) GO TO ENTER-SP-LAWCODE.

           MOVE VDUNUM0 TO SP-LAWCODE.

       READ-SPFILE.
           PERFORM OPEN-SP1-FILE.
           PERFORM READ-SP1-FILE.
           PERFORM CLOSE-SP1-FILE.
           IF ( IO-BAD )
              GO TO BEGIN-ENTER-PRIMARY-KEY.

           MOVE SP-REC TO ORIGINAL-SP-REC.
           PERFORM DISPLAY-FIELDS.
           MOVE ZEROES TO NEW-FLAG.

           IF ( NEW-REC = "Y" )
              MOVE 1 TO NEW-FLAG.

       EXIT-ENTER-PRIMARY-KEY.
           EXIT.

      ******************************************************************
       ENTER-SCANKEY SECTION.
      ******************************************************************

           MOVE 0 TO SP-SPRCLASS SP-SUBCLASS SP-LAWCODE FIRST-SCAN.

           MOVE " ENTER STATE CODE, OR F6 TO SCAN" TO MESS.
           PERFORM CALL-TMMSG.
           PERFORM OPEN-SP1-FILE.
           PERFORM START-SP1-FILE.

       ENTER-KEY.

           MOVE "E RA020SCANKEY." TO VDU.
           PERFORM SCREENIO.

           IF (        F1-KEY ) MOVE "CLR" TO IO-TYPE
                                GO TO END-ENTER-SCANKEY.

           IF (        F6-KEY ) GO TO READ-FILES-NEXT.
           IF NOT ( ENTER-KEY ) GO TO ENTER-KEY.

           IF ( VDUBUF = SPACES )
              IF ( FIRST-SCAN NOT = ZEROES )
                 MOVE SP-REC  TO ORIGINAL-SP-REC
                 GO TO END-ENTER-SCANKEY
              ELSE
                 GO TO ENTER-KEY.

           MOVE VDUBUF TO SP-ORGST.
           MOVE ZEROES TO SP-SPRCLASS SP-SUBCLASS SP-LAWCODE.
           PERFORM START-SP1-FILE.

       READ-FILES-NEXT.

           PERFORM READ-SP1-FILE-NEXT.
           IF ( IO-BAD )
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTER-SCANKEY.

      *    UNLOCK SPA-FILE.
      *    UNLOCK SPB-FILE.
      *    UNLOCK SPC-FILE.
           PERFORM DISPLAY-SCAN-KEY.
           PERFORM DISPLAY-FIELDS.
           MOVE 1 TO FIRST-SCAN.
           GO TO ENTER-KEY.

       DISPLAY-SCAN-KEY.

           MOVE "S  A020SPNO." TO VDU.
           MOVE SP-ORGST       TO VDUBUF.
           PERFORM SCREENIO.

           PERFORM VERIFY-STATE-CODE.

           MOVE "SN N020CLASS." TO VDU.
           MOVE SP-SPRCLASS     TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SN N020SUBCLASS." TO VDU.
           MOVE SP-SUBCLASS        TO VDUNUM0.
           PERFORM SCREENIO.

           PERFORM VERIFY-LOAN-CLASS.
           PERFORM DISPLAY-SUBCLASS-DESC.

           MOVE "SN N020SPCLASS." TO VDU.
           MOVE SP-LAWCODE        TO VDUNUM0.
           PERFORM SCREENIO.

       END-ENTER-SCANKEY.

           PERFORM CLOSE-SP1-FILE.
           MOVE SPACES TO MESS.
           PERFORM CALL-TMMSG.

       EXIT-ENTER-SCANKEY.
           EXIT.

      ******************************************************************
      *
      *    E N T R Y - M O D U L E
      *
      *    NEW-FLAG = 0 - MODIFY EXISTING RECORD
      *               1 - INITIAL NEW RECORD ENTRY
      *               2 - MODIFY NEW RECORD
      ******************************************************************
       ENTRY-MODULE SECTION.

           MOVE SPACES TO IO-TYPE.
           MOVE 1      TO ONE.
           MOVE ZEROES TO ELE.

       CREATE-ROUTINE.

           IF ( NEW-FLAG NOT = 1 )
              GO TO MODIFY-ROUTINE.

           ADD ONE TO ELE.

           IF ( ELE < 1           ) MOVE 1 TO ELE ONE.
           IF ( ELE > MAX        ) GO TO ENTER-ELE.

           MOVE 2 TO NEW-FLAG.

       MODIFY-ROUTINE.

           MOVE "R RA060SEL." TO VDU.
           PERFORM SCREENIO.

           IF (F1-KEY )
              MOVE "CLR" TO IO-TYPE
              MOVE "N" TO NEW-REC
              GO TO END-ENTRY-MODULE.
           IF (F2-KEY )
              MOVE SP1-KEY      TO MEMOSP-SP1-KEY
              MOVE "WI/MEMOSP" TO FORM-NAM
              CALL FORM-PROGX USING FORM-PATH MEMOSP-BUF EXIT-PATHNAME
              CANCEL FORM-PROGX
              GO TO MODIFY-ROUTINE.
           IF (F5-KEY )
              PERFORM TOGGLE-TRCDS-F01
              GO TO MODIFY-ROUTINE.
           IF NOT ( ENTER-KEY )
              GO TO MODIFY-ROUTINE.

           MOVE VDUBUF TO ACTION.

           IF ( ACTION = SPACES )
              IF ( IO-FG NOT = ZEROES )
                 GO TO MODIFY-ROUTINE
              ELSE
                 MOVE "RWR" TO IO-TYPE
                 GO TO END-ENTRY-MODULE.

      * FORM-NUM1 CHECKS FOR PAGES 1 THRU 9
           IF ( FORM-KEY1 = SPACES ) AND
              ( FORM-P1   = "P   " ) AND
              ( FORM-SEL1 NUMERIC  )
              IF ( FORM-SEL1 <  1 ) OR
                 ( FORM-SEL1 = 23 )
                 GO TO MODIFY-ROUTINE
              ELSE
                 GO TO CHECK-PAGE.

      * FORM-NUM2 CHECKS FOR PAGE 10
           IF ( FORM-KEY2 = SPACES ) AND
              ( FORM-P2   = "P"    ) AND
              ( FORM-SEL2 NUMERIC  )
              IF NOT ( FORM-2-DIGITS )
                 GO TO MODIFY-ROUTINE
              ELSE
                 GO TO CHECK-PAGE.

           GO TO CHECK-CHNO.

       CHECK-PAGE.
           MOVE "RWR" TO IO-TYPE.
           GO TO EXIT-ENTRY-MODULE.

      *-----------------------------------------------------------------
       CHECK-CHNO.

           INSPECT ACTION REPLACING ALL SPACES BY ZEROES.

           IF ( CHNO-X NOT NUMERIC ) GO TO MODIFY-ROUTINE.
           IF ( CHNO     = ZEROES  ) GO TO MODIFY-ROUTINE.
           IF ( CHNO     > MAX     ) GO TO MODIFY-ROUTINE.

           MOVE CHNO TO ELE.

      *-----------------------------------------------------------------
       ENTER-ELE.


           IF ( CHKEY    NOT = ZEROES ) AND
              ( CHFG(ELE)    = SPACES ) AND
              ( NEW-FLAG NOT = 1      )
              GO TO MODIFY-ROUTINE.

           IF ( CHFG(ELE) = "N" )
              IF ( NEW-FLAG = 1 )
                 GO TO CREATE-ROUTINE
              ELSE
              IF CHKEY NOT = EXT-CONAME-DAILY-PASSWORD
                 GO TO MODIFY-ROUTINE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE SPEC(ELE) TO VDU.
           MOVE ELE       TO VDUROW.
           MOVE ZEROES    TO VDUCOL   CURRENT-VDUCOL.
           MOVE "."       TO VDUDECIMAL.
           PERFORM SCREENIO.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - -{ F1-KEY )
           IF ( F1-KEY )
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - -{ F5-KEY )
           IF ( F5-KEY )
              IF ( ELE          = 1 ) AND
                 ( NEW-FLAG NOT = 1 )
                 PERFORM TOGGLE-TRCDS-F01
                 GO TO ENTER-ELE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - -{ UP-KEY )
           IF ( UP-KEY )
              IF ( NEW-FLAG = 1 )
                 MOVE -1 TO ONE
                 GO TO CREATE-ROUTINE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - -{ DOWN-KEY )
           IF ( DOWN-KEY )
              MOVE 1 TO ONE.
              GO TO GOTO-FIELD.

      *- - - - - - - - - - - - - - - - - - - - - - - - - -{ ENTER-KEY  )
           IF ( NOT ENTER-KEY )
              GO TO ENTER-ELE.

           MOVE 1 TO ONE.

      *-----------------------------------------------------------------
       GOTO-FIELD.

           GO TO F-01 F-02 F-03 F-04 F-05 F-06
                 DEPENDING ON ELE.

           GO TO END-ENTRY-MODULE.

      *=================================================================
      *
      *    S E T - R E C O R D - E L E M E N T S
      *
      *=================================================================
       F-01.

           IF ( VDUBUF NOT = SPACES )
              GO TO F-011-CONTINUE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE SPACES TO SP-REPO-TRCD(TTF01-ELE)

           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 4

                   MOVE ZEROES         TO SP-REPO-FMNO(TTF01-ELE, SUB)

                   MOVE "S  A030000."  TO VDU
                   MOVE ELE            TO VDUROW
                   MOVE SUB            TO VDUCOL
                   MOVE "."            TO VDUDECIMAL
                   MOVE SPACES         TO VDUBUF
                   PERFORM SCREENIO

                   MOVE "S  A220000."  TO WS-F01-VDU
                   MOVE ELE            TO WS-F01-VDUROW
                   MOVE SUB            TO WS-F01-VDUCOL
                   MOVE "D"            TO WS-F01-VDUDESC-D
                   MOVE "."            TO WS-F01-VDUDECIMAL
                   MOVE WS-F01-VDU     TO VDU
                   MOVE SPACES         TO VDUBUF
                   PERFORM SCREENIO

           END-PERFORM.

           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-011-CONTINUE.

           MOVE VDUBUF TO SP-REPO-TRCD(TTF01-ELE).
           MOVE +1     TO ONE-F01.

      *-----------------------------------------------------------------
       F-011.

           ADD  ONE-F01 TO CURRENT-VDUCOL.
           MOVE ZEROES  TO ONE-F01.

           IF ( CURRENT-VDUCOL = ZEROES )
              GO TO ENTER-ELE
           ELSE
           IF ( CURRENT-VDUCOL > 4 )
              GO TO CREATE-ROUTINE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "ENNN030000."  TO VDU.
           MOVE ELE            TO VDUROW.
           MOVE CURRENT-VDUCOL TO VDUCOL.
           MOVE "."            TO VDUDECIMAL.
           PERFORM SCREENIO.

           IF (        UP-KEY ) MOVE -1 TO ONE-F01
                                GO TO F-011.
           IF (        F1-KEY ) GO TO ENTER-ELE.
           IF ( NOT ENTER-KEY ) GO TO F-011.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    IF ( VDUNUM0 = ZEROES )
      *KC->   GO TO F-011.

           MOVE VDUNUM0 TO FD-FMNO.

           IF ( FD-FMNO = ZEROES )
              MOVE ZEROES  TO IO-FG
              MOVE SPACES  TO FD-DESC
           ELSE
              PERFORM GET-FD1-RECORD.

           MOVE "S  A220000."  TO WS-F01-VDU.
           MOVE ELE            TO WS-F01-VDUROW.
           MOVE CURRENT-VDUCOL TO WS-F01-VDUCOL.
           MOVE "D"            TO WS-F01-VDUDESC-D.
           MOVE "."            TO WS-F01-VDUDECIMAL.
           MOVE WS-F01-VDU     TO VDU.
           MOVE FD-DESC        TO VDUBUF.
           PERFORM SCREENIO.

           IF ( IO-BAD )
              GO TO F-011.

           MOVE FD-FMNO TO SP-REPO-FMNO(TTF01-ELE, CURRENT-VDUCOL).

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE +1 TO ONE-F01.
           GO TO F-011.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       F-02.

           IF ( VDUBUF NOT = SPACES )
              GO TO F-021-CONTINUE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE SPACES TO SP-AUTO-SEND-TRCD(1).

           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 4

              MOVE 0 TO SP-AUTO-SEND-FMNO(1, SUB)
              MOVE "S  A030000."  TO VDU
              MOVE ELE            TO VDUROW
              MOVE SUB            TO VDUCOL
              MOVE "."            TO VDUDECIMAL
              MOVE SPACES         TO VDUBUF
              PERFORM SCREENIO

              MOVE "S  A220000."  TO WS-F01-VDU
              MOVE ELE            TO WS-F01-VDUROW
              MOVE SUB            TO WS-F01-VDUCOL
              MOVE "D"            TO WS-F01-VDUDESC-D
              MOVE "."            TO WS-F01-VDUDECIMAL
              MOVE WS-F01-VDU     TO VDU
              MOVE SPACES         TO VDUBUF
              PERFORM SCREENIO

           END-PERFORM.

           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-021-CONTINUE.

           MOVE VDUBUF TO SP-AUTO-SEND-TRCD(1).
           MOVE +1     TO ONE-F01.

      *-----------------------------------------------------------------
       F-021.

           ADD  ONE-F01 TO CURRENT-VDUCOL.
           MOVE ZEROES  TO ONE-F01.

           IF ( CURRENT-VDUCOL = ZEROES )
              GO TO ENTER-ELE
           ELSE
           IF ( CURRENT-VDUCOL > 4 )
              GO TO CREATE-ROUTINE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "ENNN030000."  TO VDU.
           MOVE ELE            TO VDUROW.
           MOVE CURRENT-VDUCOL TO VDUCOL.
           MOVE "."            TO VDUDECIMAL.
           PERFORM SCREENIO.

           IF (        UP-KEY ) MOVE -1 TO ONE-F01
                                GO TO F-021.
           IF (        F1-KEY ) GO TO ENTER-ELE.
           IF ( NOT ENTER-KEY ) GO TO F-021.

           MOVE VDUNUM0 TO FD-FMNO.
           IF ( FD-FMNO = ZEROES )
              MOVE ZEROES  TO IO-FG
              MOVE SPACES  TO FD-DESC
           ELSE
              PERFORM GET-FD1-RECORD.

           MOVE "S  A220000."  TO WS-F01-VDU.
           MOVE ELE            TO WS-F01-VDUROW.
           MOVE CURRENT-VDUCOL TO WS-F01-VDUCOL.
           MOVE "D"            TO WS-F01-VDUDESC-D.
           MOVE "."            TO WS-F01-VDUDECIMAL.
           MOVE WS-F01-VDU     TO VDU.
           MOVE FD-DESC        TO VDUBUF.
           PERFORM SCREENIO.
           IF ( IO-BAD )
              GO TO F-021.

           MOVE FD-FMNO TO SP-AUTO-SEND-FMNO(1, CURRENT-VDUCOL).

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE +1 TO ONE-F01.
           GO TO F-021.

      *-----------------------------------------------------------------
       F-03.

           IF ( VDUBUF NOT = SPACES )
              GO TO F-031-CONTINUE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE SPACES TO SP-AUTO-SEND-TRCD(2).

           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 4

              MOVE 0 TO SP-AUTO-SEND-FMNO(2, SUB)
              MOVE "S  A030000."  TO VDU
              MOVE ELE            TO VDUROW
              MOVE SUB            TO VDUCOL
              MOVE "."            TO VDUDECIMAL
              MOVE SPACES         TO VDUBUF
              PERFORM SCREENIO

              MOVE "S  A220000."  TO WS-F01-VDU
              MOVE ELE            TO WS-F01-VDUROW
              MOVE SUB            TO WS-F01-VDUCOL
              MOVE "D"            TO WS-F01-VDUDESC-D
              MOVE "."            TO WS-F01-VDUDECIMAL
              MOVE WS-F01-VDU     TO VDU
              MOVE SPACES         TO VDUBUF
              PERFORM SCREENIO

           END-PERFORM.

           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-031-CONTINUE.

           MOVE VDUBUF TO SP-AUTO-SEND-TRCD(2).
           MOVE +1     TO ONE-F01.

      *-----------------------------------------------------------------
       F-031.

           ADD  ONE-F01 TO CURRENT-VDUCOL.
           MOVE ZEROES  TO ONE-F01.

           IF ( CURRENT-VDUCOL = ZEROES )
              GO TO ENTER-ELE
           ELSE
           IF ( CURRENT-VDUCOL > 4 )
              GO TO CREATE-ROUTINE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "ENNN030000."  TO VDU.
           MOVE ELE            TO VDUROW.
           MOVE CURRENT-VDUCOL TO VDUCOL.
           MOVE "."            TO VDUDECIMAL.
           PERFORM SCREENIO.

           IF (        UP-KEY ) MOVE -1 TO ONE-F01
                                GO TO F-021.
           IF (        F1-KEY ) GO TO ENTER-ELE.
           IF ( NOT ENTER-KEY ) GO TO F-021.

           MOVE VDUNUM0 TO FD-FMNO.
           IF ( FD-FMNO = ZEROES )
              MOVE ZEROES  TO IO-FG
              MOVE SPACES  TO FD-DESC
           ELSE
              PERFORM GET-FD1-RECORD.

           MOVE "S  A220000."  TO WS-F01-VDU.
           MOVE ELE            TO WS-F01-VDUROW.
           MOVE CURRENT-VDUCOL TO WS-F01-VDUCOL.
           MOVE "D"            TO WS-F01-VDUDESC-D.
           MOVE "."            TO WS-F01-VDUDECIMAL.
           MOVE WS-F01-VDU     TO VDU.
           MOVE FD-DESC        TO VDUBUF.
           PERFORM SCREENIO.
           IF ( IO-BAD )
              GO TO F-031.

           MOVE FD-FMNO TO SP-AUTO-SEND-FMNO(2, CURRENT-VDUCOL).

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE +1 TO ONE-F01.
           GO TO F-031.

       F-04.
           MOVE VDUNUM0 TO SP-PERCENT-REGPAY-AVAIL.
           GO TO CREATE-ROUTINE.
       F-05.
           IF NOT (VDUBUF = "Y" OR "N")
              GO TO ENTER-ELE.

           MOVE VDUBUF TO SP-NON-LOAN-FLAG.
           GO TO CREATE-ROUTINE.
       F-06.
           IF NOT (VDUBUF = "Y" OR "N")
              GO TO ENTER-ELE.

           MOVE VDUBUF TO SP-SKIP-TRW.
           GO TO CREATE-ROUTINE.

       END-ENTRY-MODULE.

           MOVE ZEROES TO FORM-SEL1 FORM-SEL2.

       EXIT-ENTRY-MODULE.
           EXIT.

      ******************************************************************
      *
      *    W R I T E - M O D U L E
      *
      ******************************************************************
       WRITE-MODULE SECTION.
      *-----------------------------------------------------------------
       REWRITE-RECORD.

           PERFORM OPEN-SP1-FILE.
           MOVE SP-REC TO CHANGED-SP-REC.

           IF ( ORIGINAL-SP-REC = CHANGED-SP-REC )
              GO TO END-WRITE-MODULE.

           PERFORM READ-SP1-FILE.
           IF ( IO-BAD )
              PERFORM UNDERGONE-CHANGE
              PERFORM RECORD-DELETED
              GO TO END-WRITE-MODULE.

           IF ( SP-REC NOT = ORIGINAL-SP-REC )
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

           GO TO END-WRITE-MODULE.

       END-WRITE-MODULE.
           PERFORM CLOSE-SP1-FILE.

       EXIT-WRITE-MODULE.
           EXIT.

      ******************************************************************
       DISPLAY-FIELDS SECTION.
      ******************************************************************

           MOVE SPACES TO DISPLAY-BUF.

           MOVE "S  8000EFFDATE." TO VDU.
           MOVE SP-EFFDATE        TO VDU-DATE-YYYYMMDD.
           PERFORM SCREENIO.

           MOVE "S  8000EXPDATE." TO VDU.
           MOVE SP-EXPDATE        TO VDU-DATE-YYYYMMDD.
           PERFORM SCREENIO.

           PERFORM DISPLAY-SUBCLASS-DESC.

           MOVE SP-REPO-TRCD(1)               TO F01.

           MOVE SP-AUTO-SEND-TRCD(1) TO F02.
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 4
              MOVE SP-AUTO-SEND-FMNO(1, SUB) TO F021(SUB) FD-FMNO
              IF FD-FMNO NOT = 0 
                 PERFORM GET-FD1-RECORD
                 MOVE FD-DESC TO F021D(SUB)
              END-IF
           END-PERFORM.

           MOVE SP-AUTO-SEND-TRCD(2) TO F03.
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 4
              MOVE SP-AUTO-SEND-FMNO(2, SUB) TO F031(SUB) FD-FMNO
              IF FD-FMNO NOT = 0
                 PERFORM GET-FD1-RECORD
                 MOVE FD-DESC TO F031D(SUB)
              END-IF
           END-PERFORM.

           MOVE SP-PERCENT-REGPAY-AVAIL TO F04.
           MOVE SP-NON-LOAN-FLAG        TO F05.
           MOVE SP-SKIP-TRW             TO F06.

           MOVE DISPLAY-BUF TO WR-BUF.
           MOVE DISPLAY-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

           MOVE ZEROES TO TTF01-ELE.
           PERFORM TOGGLE-TRCDS-F01.

       DISPLAY-FIELDS-EXIT.
           EXIT.

      ******************************************************************
       SCAN-LC-FILE SECTION.
      ******************************************************************

           MOVE 0      TO LCSCAN-KEY.
           MOVE SPACES TO LCSCAN-VERIFY.
           MOVE "WI/LCSCAN" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH LCSCAN-BUF EXIT-PATHNAME.
           CANCEL FORM-PROGX.
           IF LCSCAN-STATUS NOT = "00" OR LCSCAN-KEY = 0
              MOVE 9 TO IO-FG
              GO TO EXIT-SCAN-LC-FILE.

           MOVE ZEROES TO IO-FG.

       EXIT-SCAN-LC-FILE.
           EXIT.

      ******************************************************************
       SCAN-SP-FILE SECTION.
      ******************************************************************

           MOVE SPACES     TO SPSCAN-SP1-KEY.
           MOVE SPACES     TO SPSCAN-LAST-LAW.
           MOVE "WI/SPSCAN" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH SPSCAN-BUF EXIT-PATHNAME.
           CANCEL FORM-PROGX.
           IF SPSCAN-STATUS NOT = "00" OR SPSCAN-SP1-KEY = SPACES
              MOVE 9 TO IO-FG
              GO TO EXIT-SCAN-SP-FILE
           ELSE
              MOVE ZEROES TO IO-FG.

       EXIT-SCAN-SP-FILE.
           EXIT.

      ******************************************************************
       TOGGLE-TRCDS-F01 SECTION.
      ******************************************************************

           ADD 1 TO TTF01-ELE.

           IF ( NOT TTF01-ELE-VALID )
              MOVE 1 TO TTF01-ELE.

           MOVE SPACES TO TOGGLE-TRCDS-F01-BUF.

           MOVE SP-REPO-TRCD(TTF01-ELE)  TO TTF01-CURRENT-TRCD.

           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 4

               IF ( SP-REPO-TRCD(TTF01-ELE) = SPACES )
                  MOVE SPACES    TO TTF01-CURRENT-FMNO     (SUB)
                                    TTF01-CURRENT-FMNO-DESC(SUB)
               ELSE
                  MOVE SP-REPO-FMNO(TTF01-ELE, SUB)
                                 TO TTF01-CURRENT-FMNO     (SUB)
                                    FD-FMNO
                  IF ( FD-FMNO NOT = ZEROES )
                     PERFORM GET-FD1-RECORD
                     MOVE FD-DESC TO TTF01-CURRENT-FMNO-DESC(SUB)
                  ELSE
                     MOVE SPACES  TO TTF01-CURRENT-FMNO-DESC(SUB)
                  END-IF
               END-IF

           END-PERFORM.

       TOGGLE-TRCDS-ALL-TRCDS.

           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 3

               IF ( SP-REPO-TRCD(SUB) = SPACES )
                  MOVE "  "              TO TTF01-ALL-TRCD(SUB)
               ELSE
                  MOVE SP-REPO-TRCD(SUB) TO TTF01-ALL-TRCD(SUB)
               END-IF

           END-PERFORM.

           IF ( TTF01-ELE = 01 )
              MOVE ":FR " TO TTF01-01-ATTR
              MOVE ":R  " TO TTF01-02-ATTR
              MOVE ":R  " TO TTF01-03-ATTR
           ELSE
           IF ( TTF01-ELE = 02 )
              MOVE ":R  " TO TTF01-01-ATTR
              MOVE ":FR " TO TTF01-02-ATTR
              MOVE ":R  " TO TTF01-03-ATTR
           ELSE
           IF ( TTF01-ELE = 03 )
              MOVE ":R  " TO TTF01-01-ATTR
              MOVE ":R  " TO TTF01-02-ATTR
              MOVE ":FR " TO TTF01-03-ATTR.

           MOVE TOGGLE-TRCDS-F01-BUF TO WR-BUF.
           MOVE TOGGLE-TRCDS-F01-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       TOGGLE-TRCDS-F01-EXIT.
           EXIT.

      **************************************************************************
       FORM-RESET SECTION.
      **************************************************************************
           MOVE EXT-CONAME-CONAME TO SPMANN-CONAME.
           MOVE EXT-CONAME-SYSDATE TO SPMANN-SYSDATE.
           DISPLAY SPACES UPON SPMAIN-WINDOW-HANDLE
                               LINE 0 COL 0 ERASE SCREEN.
           DISPLAY SPMANN-SCREEN UPON SPMAIN-WINDOW-HANDLE.
           MOVE SPMANN-FORM-FIELDS TO WR-BUF.
           MOVE SPMANN-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      **************************************************************************
       SETUP-LINK-INFO SECTION.
      **************************************************************************
           MOVE SPMANN-HELP--FILE TO HELP-LINK-FILE.
           MOVE SPMANN-HELP--DIR TO HELP-LINK-DIR.

      **************************************************************************
       VDU-CONVERT-FIELD SECTION.
      **************************************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBMN/SPMANN_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ******************************************************************
      *
      *    C O P Y - L I B R A R Y - S E C T I O N S
      *
      ******************************************************************
       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBLP/LPTBNO.CPY".

      *****************************************************************
       MISC-PARAGRAPHS SECTION.
      *****************************************************************
       COPY "LIBGB/ACCESS.CPY".
       DISPLAY-SUBCLASS-DESC.

           MOVE "S  A150DESC. " TO VDU.
           MOVE SP-DESC         TO VDUBUF.
           PERFORM SCREENIO.

      *-----------------------------------------------------------------
       GET-FD1-RECORD.
      * FOR SOME REASON, SP-AUTO-SEND-FMNO HAS SPACES
           IF FD-FMNO NOT = "0   "
              PERFORM OPEN-FD1-FILE
              PERFORM READ-FD1-FILE
              PERFORM CLOSE-FD1-FILE
              IF ( IO-BAD )
                 MOVE "*** NOT ON FILE ***" TO FD-DESC.

      *-----------------------------------------------------------------
       VERIFY-STATE-CODE.
           MOVE SP-ORGST TO ST-CODE.
           PERFORM OPEN-ST1-FILE.
           PERFORM READ-ST1-FILE.
           PERFORM CLOSE-ST1-FILE.

           IF ( IO-FG NOT = ZEROES )
              MOVE "* NOT ON FILE *" TO ST-DESC.

           MOVE "S  A160STDESC." TO VDU.
           MOVE ST-DESC          TO VDUBUF.
           PERFORM SCREENIO.

      *-----------------------------------------------------------------
       VERIFY-LOAN-CLASS.
           MOVE SP-SPRCLASS TO LC-CODE.
           PERFORM OPEN-LC1-FILE.
           PERFORM READ-LC1-FILE.
           PERFORM CLOSE-LC1-FILE.

           IF ( IO-FG NOT = ZEROES )
              MOVE "* NOT ON FILE *" TO LC-DESC.

           MOVE "S  A160LCDESC. " TO VDU.
           MOVE LC-DESC           TO VDUBUF.
           PERFORM SCREENIO.

      *-----------------------------------------------------------------
       RECORD-DELETED.
           MOVE "RECORD WAS DELETED!" TO MESS.
           PERFORM SEND-MESS.

      *-----------------------------------------------------------------
       RECORD-CHANGED.
           MOVE "CHANGES WERE MADE TO THIS RECORD!" TO MESS.
           PERFORM SEND-MESS.

      *-----------------------------------------------------------------
       CHANGE-AGAIN.
           MOVE "PLEASE RE-INPUT YOUR CHANGES!" TO MESS.
           PERFORM SEND-MESS.

      *-----------------------------------------------------------------
       ALREADY-CREATED.
           MOVE "RECORD HAS BEEN ALREADY CREATED!" TO MESS.
           PERFORM SEND-MESS.

      *-----------------------------------------------------------------
       UNDERGONE-CHANGE.
           MOVE "THIS RECORD HAS UNDERGONE CHANGE BY ANOTHER USER,"
             TO MESS.
           PERFORM SEND-MESS.

      *-----------------------------------------------------------------
       ACTIVE-RECORD.
           MOVE "ACTIVE CUSTOMER CANNOT BE DELETED" TO MESS.
           PERFORM SEND-MESS.

      *-----------------------------------------------------------------
       NOT-EXPIRED.
           MOVE "THIS RECORD HAS NOT EXPIRED - CANNOT DELETE" TO MESS.
           PERFORM SEND-MESS.

      ******************************************************************
       IO-ROUTINES SECTION.
      ******************************************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-ST1-FILE.
           PERFORM CLOSE-SP1-FILE.
           PERFORM CLOSE-LC1-FILE.
           PERFORM CLOSE-FD1-FILE.
       COPY "LIBLP/LPSTGS_SQL.CPY".
       COPY "LIBLP/LPSPGS_SQL.CPY".
       COPY "LIBLP/LPLCGS_SQL.CPY".
       COPY "LIBLP/LPFDGS_SQL.CPY".
       COPY "LIBLP/LPLC1RN.CPY".
       COPY "LIBLP/LPSP1IN.CPY".
       COPY "LIBLP/LPST1RN.CPY".
       COPY "LIBLP/LPFD1RN.CPY".

       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO ERRCD.

      ******************************************************************
       END-PROGRAM SECTION.
      ******************************************************************
           PERFORM CLOSE-FILES.

       EXIT-PROG.
       STOP-RUN.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY SPMANN-SCREEN.
           MOVE FORM-PATH TO FORM-PATHNAME.
           MOVE SPMAIN-LINK TO FORM-LINK-AREA.
           EXIT PROGRAM.
