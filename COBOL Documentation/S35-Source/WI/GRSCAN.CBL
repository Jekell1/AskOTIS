       IDENTIFICATION DIVISION.
       PROGRAM-ID.      GRSCAN.
      ******************************************************************
      *
      *  GROUP ID LOCATION FILE SCAN
      *
      * REV:
      *  JFF 120895 ADDED MORE INFORMATION SCAN WINDOW
      *  MJD 011196 ADDED PASSWORD TO MORE INFO WINDOW ( SEQ 0 )
      *             REMOVED OPEN/CLOSE OF GR FILE IN MORE INFO
      *             SECTION. (IT WAS CAUSING PROBLEMS WITH CODE
      *             SCAN TRYING TO READ CLOSED FILE).
      *  KEC 031198 ADDED GR-LN-APPROVAL-LIMIT TO 'MORE INFORMATION'
      *              SCAN WINDOW
      *   CS 141030 ADD MOVE OF FLDSEL-KEY TO GRSCAN-KEY BEFORE FOR MORE INFO
      * KEC 2016.0311 CHANGED CD-LV???? TO CD-LV-????
      * KEC 2016.0328 CHANGED CDSCAN-WINDOW-BUF TO HAVE MATCHING CD1-KEY
      *               LAYOUTS (FOR EACH CODE TYPE; CD-TYPE).
      *               CDSCAN-CD1-KEY IS THE NEW (PRIMARY) WITH REDEFINES OF
      *               CDSCAN-??-KEY (NEW).
      * BAH 190128 SPLIT CDFILE
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
           "@(#)WI/GRSCAN.CBL S35 02/23/24-11:41:41 >".
       COPY "LIBGB/GB01GR.CPY".
       COPY "LIBGB/GB01GR_SQL.CPY".
       COPY "LIBGB/GBWSGR.CPY".
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBLP/LP01CD.CPY".
       COPY "LIBLP/LP01CD_SQL.CPY".
       COPY "LIBLP/LPWSCD.CPY".

       01  GRSCAN-LINES         PIC 99  VALUE 22.
       01  GRSCAN-COLUMNS       PIC 99  VALUE 42.
       01  GRSCAN-BEGIN-Y       PIC 99  VALUE 2.
       01  GRSCAN-BEGIN-X       PIC 99  VALUE 36.

       01  ERRCD           PIC X VALUE SPACES.

       01  FIRST-TIME      PIC X.
       01  SUB             PIC 99.
       01  LISTSUB         PIC 999 VALUE 0.

       01  SPECIFIC-LEVEL  PIC 99 VALUE 0.

       01  FLDSEL-FIELD    PIC X(4)  VALUE "010.".
       01  FLDSEL-STAT     PIC XX.
       01  FLDSEL-LIST.
           03  FILLER      PIC X(40)  VALUE
               "010 020 030 040 050 060 070 080 090 100 ".
           03  FILLER      PIC X(20)  VALUE
               "110 120 130 140 150.".
       01  FLDSEL-BUF.
           03  FLDSEL-KEY       PIC X(4).
           03  FILLER           PIC X.
           03  FLDSEL-LOC       PIC Z999.
           03  FILLER           PIC XX.
           03  FLDSEL-DESC      PIC X(25).
           03  FILLER           PIC X.
           03  FLDSEL-LEVEL     PIC 9.

       01  FIRST-FIELD          PIC X(4)  VALUE "010.".
       01  LAST-FIELD           PIC X(4)  VALUE "150.".

       01  DISPLAY-LIST.
           03  FILLER      PIC X(40)  VALUE
               "010 020 030 040 050 060 070 080 090 100 ".
           03  FILLER      PIC X(31)  VALUE
               "110 120 130 140 150 PGNO MAXPG.".
       01  DISPLAY-BUF.
           03  D-LINE      OCCURS 15 TIMES.
               05  D-GROUP-ID  PIC X(4).
               05  FILLER      PIC X.
               05  D-LOC       PIC Z999.
               05  D-LEV       REDEFINES D-LOC
                               PIC ZZZ9.
               05  FILLER      PIC XX.
               05  D-DESC      PIC X(25).
               05  FILLER      PIC X.
               05  D-LEVEL     PIC 9.
           03  D-PAGE-NO       PIC ZZ9.
           03  D-MAX-PAGE      PIC ZZ9.

       01  PAGE-KEY-TABLE.
           03  PAGE-KEY    PIC X(4)   OCCURS 100.
       01  PAGE-NUMBER     PIC 999.
       01  MAX-PAGES       PIC 999.

      ********************************************
      *    MORE INFO
      ********************************************
       01  MORE-FG         PIC X      VALUE " ".
       01  MORE-LIST.
           03  FILLER      PIC X(50)  VALUE
           "GRID GRNAME GRLEVEL GRDESC ADDR1 ADDR2 CITY STATE ".
           03  FILLER      PIC X(44)  VALUE
           "ZIP PHONE GRCONTACT HOMEBR APP-LIMIT BRNAME.".
       01  MORE-BUF.
           03  M-GROUP     PIC X(4).
           03  M-GRNAME    PIC X(20).
           03  M-GRLEVEL   PIC 9.
           03  M-LDESC     PIC X(15).
           03  M-ADR1      PIC X(20).
           03  M-ADR2      PIC X(20).
           03  M-CITY      PIC X(15).
           03  M-STATE     PIC X(4).
           03  M-ZIP       PIC X(10).
           03  M-PHONE     PIC X(14).
           03  M-CONTACT   PIC X(20).
           03  M-HOMEBR    PIC 9(4).
           03  M-APP-LIMIT PIC Z,ZZZ,ZZ9.
           03  M-BRNAME    PIC X(17).

       01  MORE-LIST2.
           03  FILLER      PIC X(50)  VALUE
               "0901 0902 0903 0904 0905 0906 0907 0908 0909 0910 ".
           03  FILLER      PIC X(50)  VALUE
               "0911 0912 0913 0914 0915 0916 0917 0918 0919 0920 ".
           03  FILLER      PIC X(50)  VALUE
               "0921 0922 0923 0924 0925 0926 0927 0928 0929 0930 ".
           03  FILLER      PIC X(50)  VALUE
               "0931 0932 0933 0934 0935 0936 0937 0938 0939 0940 ".
           03  FILLER      PIC X(50)  VALUE
               "0941 0942 0943 0944 0945 0946 0947 0948 0949 0950 ".
           03  FILLER      PIC X(50)  VALUE
               "0951 0952 0953 0954 0955 0956 0957 0958 0959 0960 ".
           03  FILLER      PIC X(50)  VALUE
               "0961 0962 0963 0964 0965 0966 0967 0968 0969 0970 ".
           03  FILLER      PIC X(50)  VALUE
               "0971 0972 0973 0974 0975 0976 0977 0978 0979 0980 ".
           03  FILLER      PIC X(50)  VALUE
               "0981 0982 0983 0984 0985 0986 0987 0988 0989 0990 ".
           03  FILLER      PIC X(50)  VALUE
               "0991 0992 0993 0994 0995 0996 0997 0998 0999 0900.".
       01  MORE-BUF2.
           03  LIST-TABLE  PIC X(4)   OCCURS 100.


       COPY "LIBWI/PFSWINDOWSW.CPY".
       COPY "LIBGB/PHON400.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       01  GRSCAN-WINDOW-HANDLE PIC X(10).
       COPY "LIBWI/CDSCANW.CPY".
       COPY "LIBGB/PASSWDW.CPY".
       01  MORE-SEQ        PIC 9      VALUE 0.
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBWI/GRSCAN_DEF.CPY".
       COPY "LIBWI/GRSCN2_DEF.CPY".
       COPY "LIBWI/GRSCAN_WKS.CPY".
       COPY "LIBWI/GRSCN2_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/FLDSELW.CPY".

       COPY "LIBGB/GETFMW.CPY".
       COPY "LIBWI/GRSCANW.CPY".

       SCREEN SECTION.
       COPY "LIBWI/GRSCAN_SCN.CPY".
       COPY "LIBWI/GRSCN2_SCN.CPY".


       PROCEDURE DIVISION
           USING FORM-PATHNAME GRSCAN-BUF EXIT-PATHNAME.

      *************************************************
       MAIN-PROGRAM SECTION.
      *************************************************

           PERFORM SQL-CONNECT.

       COPY "LIBGB/CHKSEC.CPY".
      D    STOP MESS.

           MOVE "XX" TO GRSCAN-STATUS.
           MOVE " " TO VDU-WIMENU-SELECTION.

           MOVE "GRSCAN" TO VDU-FORM-NAME.

      * AT SCREEN LINE USES THE TERMINAL POSITION, NOT THE CURRENT WINDOW OPEN
           DISPLAY FLOATING GRAPHICAL WINDOW
                   AT  SCREEN LINE GRSCAN-BEGIN-Y * VDU-WIN-L-OFFSET
                   AT  SCREEN COL  GRSCAN-BEGIN-X * VDU-WIN-C-OFFSET
                   SIZE    GRSCAN-COLUMNS + VDU-WIN-W-MOD
                   LINES   GRSCAN-LINES + VDU-WIN-H-MOD
           COLOR IS VDU-WCOLOR-SCAN,CONTROL VALUE IS VDU-WCTRL-SCAN
                   CELL SIZE IS LABEL FONT
                   BLANK SCREEN
                   HANDLE  IS GRSCAN-WINDOW-HANDLE.

           PERFORM FORM-RESET.

           MOVE "Y" TO FIRST-TIME.
           MOVE 1 TO MAX-PAGES.

           MOVE SPACES TO GR1-KEY.
           PERFORM OPEN-GR1-FILE.
           PERFORM START-GR1-FILE.
           PERFORM READ-GR1-FILE-NEXT.
           IF IO-FG NOT = 0
              MOVE "NO GROUP ID'S ON FILE!" TO D-LINE(1)
              MOVE DISPLAY-BUF TO WR-BUF
              MOVE DISPLAY-LIST TO WR-LIST
              PERFORM CALL-WRFORM
              MOVE "E  A010150." TO VDU
              PERFORM SCREENIO
              MOVE SPACES TO GRSCAN-KEY
              MOVE "1>" TO FLDSEL-STAT
              GO TO END-ROUTINE.

       ENTER-LEVEL.
           MOVE "S  A000LEVX." TO VDU.
           PERFORM SCREENIO.

           PERFORM DISPLAY-LEVELS.
           IF FIRST-TIME = "Y"
              MOVE "F7, F3-ALL, ENTER GROUP LEVEL:" TO MESS
           ELSE
              MOVE "F7, F6-SCAN, F3-ALL, ENTER LEVEL:" TO MESS.
           PERFORM SEND-LEGEND.

           MOVE "EN N010LEV." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>"
              MOVE SPACES TO GRSCAN-NO
              GO TO END-ROUTINE.
           IF VDUSTATUS = "16"
              MOVE "S  A000LEVX." TO VDU
              MOVE "ALL" TO VDUBUF
              MOVE 99 TO SPECIFIC-LEVEL
              GO TO CONTINUE-LEVEL.

           IF FIRST-TIME = "N"
              IF VDUSTATUS = "1<"
                 PERFORM CODE-SCAN
                 IF IO-FG = 0
                    MOVE "00" TO VDUSTATUS
                 ELSE
                    GO TO ENTER-LEVEL.
           IF VDUSTATUS NOT = "00"
              GO TO ENTER-LEVEL.

           MOVE VDUNUM0 TO SPECIFIC-LEVEL.

           PERFORM GET-CD-LV.
           IF IO-FG = 9
              MOVE "NOT A VALID LEVEL!" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-LEVEL.

           MOVE "S  A000LEVX." TO VDU.
           MOVE CD-DESC TO VDUBUF.

       CONTINUE-LEVEL.
           PERFORM SCREENIO.
           MOVE "N" TO FIRST-TIME.

           MOVE SPACES TO GR1-KEY.
           PERFORM START-GR1-FILE.
       LOAD-TABLE.
           MOVE GR1-KEY TO PAGE-KEY(MAX-PAGES).
           PERFORM NEXT-VALID-GR 15 TIMES.
           IF IO-FG = 0
              IF MAX-PAGES < 100
                 ADD 1 TO MAX-PAGES
                 GO TO LOAD-TABLE.

           MOVE 1 TO PAGE-NUMBER.
           MOVE FIRST-FIELD TO FLDSEL-FIELD.

       DISPLAY-PAGE.
           MOVE SPACES TO DISPLAY-BUF.
           MOVE PAGE-NUMBER TO D-PAGE-NO.
           MOVE MAX-PAGES TO D-MAX-PAGE.
           MOVE 1 TO SUB.
           MOVE PAGE-KEY(PAGE-NUMBER) TO GR1-KEY.

           PERFORM START-GR1-FILE.
       MOVE-DISPLAY.
           PERFORM NEXT-VALID-GR.
           IF IO-FG NOT = 0
              GO TO SEND-DISPLAY.

           MOVE GR-LEVEL TO D-LEVEL(SUB).
           MOVE GR-NAME TO D-DESC(SUB).
           MOVE GR-ID TO D-GROUP-ID(SUB).
           IF GR-LEVEL = 9
              MOVE GR-MEMBER-LOC(1) TO D-LOC(SUB).
           IF SUB < 15
              ADD 1 TO SUB
              GO TO MOVE-DISPLAY.
       SEND-DISPLAY.
           IF D-GROUP-ID(1) = SPACES
              MOVE "NO GROUPS FOR THIS LEVEL!" TO D-LINE(1)
              MOVE DISPLAY-BUF TO WR-BUF
              MOVE DISPLAY-LIST TO WR-LIST
              PERFORM CALL-WRFORM
              MOVE "E  A010150." TO VDU
              PERFORM SCREENIO
              GO TO ENTER-LEVEL.

           MOVE DISPLAY-BUF TO WR-BUF.
           MOVE DISPLAY-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       SELECT-DISPLAY.
           MOVE "F7-EXIT, F1, F6, SELECT A GROUP (RTN):" TO MESS.
           PERFORM SEND-LEGEND.

           IF MORE-FG = " "
              MOVE FIRST-FIELD TO FLDSEL-FIELD.

           PERFORM CALL-FLDSEL.
           MOVE " " TO MORE-FG.
           IF ( FLDSEL-STAT = "1<" )
              MOVE MORE-SEQ TO PASSWDW-SEQ
              MOVE "TO VIEW MORE INFO" TO PASSWDW-PROMPT2
              PERFORM VERIFY-PASSWORD
              IF PASSWDW-STAT NOT = "OK"
                 GO TO SELECT-DISPLAY
              END-IF
              MOVE FLDSEL-KEY TO GRSCAN-KEY
              PERFORM MORE-INFO
              MOVE "Y" TO MORE-FG
              GO TO SELECT-DISPLAY.

           IF FLDSEL-STAT = "1U"
              IF PAGE-NUMBER = 1
                 PERFORM ALARM
                 GO TO SELECT-DISPLAY
              ELSE
                 SUBTRACT 1 FROM PAGE-NUMBER
                 MOVE LAST-FIELD TO FLDSEL-FIELD
                 GO TO DISPLAY-PAGE.

      * TEST PAGE UP
           IF FLDSEL-STAT = "1P"
              IF PAGE-NUMBER = 1
                 PERFORM ALARM
                 GO TO SELECT-DISPLAY
              ELSE
                 SUBTRACT 1 FROM PAGE-NUMBER
                 MOVE FIRST-FIELD TO FLDSEL-FIELD
                 GO TO DISPLAY-PAGE.

      * TEST PAGE DOWN OR DOWN ARROR
           IF FLDSEL-STAT = "1N" OR FLDSEL-STAT = "1D"
              IF PAGE-NUMBER NOT < MAX-PAGES
                 PERFORM ALARM
                 GO TO SELECT-DISPLAY
              ELSE
                 ADD 1 TO PAGE-NUMBER
                 MOVE FIRST-FIELD TO FLDSEL-FIELD
                 GO TO DISPLAY-PAGE.

      * TEST HOME
           IF FLDSEL-STAT = "1H"
              MOVE 1 TO PAGE-NUMBER
              MOVE FIRST-FIELD TO FLDSEL-FIELD
              GO TO DISPLAY-PAGE.

           IF FLDSEL-STAT = "10" OR FLDSEL-STAT = "1U"
              MOVE SPACES TO GRSCAN-NO
              MOVE SPACES TO DISPLAY-BUF
              MOVE DISPLAY-BUF TO WR-BUF
              MOVE DISPLAY-LIST TO WR-LIST
              PERFORM CALL-WRFORM
              GO TO ENTER-LEVEL.

           IF FLDSEL-STAT = "1>"
              MOVE SPACES TO GRSCAN-NO
              GO TO END-ROUTINE.

           IF FLDSEL-STAT NOT = "00"
              PERFORM ALARM
              GO TO SELECT-DISPLAY.

           MOVE FLDSEL-KEY TO GRSCAN-KEY.
           MOVE "00"       TO GRSCAN-STATUS.

      *    INSPECT FLDSEL-FIELD REPLACING FIRST ".  " BY ":U.".
      *    MOVE GRSCAN-WINDOW-BUF TO WR-BUF.
      *    MOVE FLDSEL-FIELD TO WR-LIST.
      *    PERFORM CALL-WRFORM.
      *    INSPECT GRSCAN-KEY REPLACING LEADING " " BY "0".

           GO TO END-ROUTINE.

      *******************************************************
       MORE-INFO SECTION.
      *******************************************************
           MOVE GRSCAN-KEY TO GR1-KEY.
           PERFORM READ-GR1-FILE.
           IF IO-FG NOT = 0
              MOVE "UNABLE TO READ GROUP RECORD" TO MESS
              PERFORM SEND-MESS
              GO TO MORE-INFO-EXIT.

           MOVE GR-HOME-BRANCH TO BR-NO.
           PERFORM OPEN-BR-FILE.
           PERFORM READ-BR-FILE.
           PERFORM CLOSE-BR-FILE.
           IF IO-FG NOT = 0
              MOVE SPACES TO M-BRNAME
           ELSE
              MOVE BR-NAME TO M-BRNAME.

           MOVE 19 TO WINDOW-LINES.
           MOVE 75 TO WINDOW-COLUMNS.
           MOVE 5 TO WINDOW-BEGIN-Y.
           MOVE 3 TO WINDOW-BEGIN-X.
           MOVE "GRSCN2" TO VDU-FORM-NAME.
           MOVE "WI/GRSCN2" TO WINDOW-FORM-NAM.

           PERFORM OPEN-WINDOW.
           DISPLAY GRSCN2-SCREEN.

           MOVE GRSCN2-FORM-FIELDS TO WR-BUF. 
           MOVE GRSCN2-FORM-LIST TO WR-LIST. 
           PERFORM CALL-WRFORM. 

           MOVE GR-ID     TO M-GROUP.
           MOVE GR-NAME   TO M-GRNAME.
           MOVE GR-LEVEL  TO M-GRLEVEL.
           MOVE CD-DESC   TO M-LDESC.
           MOVE GR-ADDR-1 TO M-ADR1.
           MOVE GR-ADDR-2 TO M-ADR2.
           MOVE GR-CITY   TO M-CITY.
           MOVE GR-STATE  TO M-STATE.
           MOVE GR-ZIP    TO M-ZIP.
           MOVE GR-PHONE  TO TEL-BUF.
           PERFORM SEND-PHONE.
           MOVE TEL-EDIT  TO M-PHONE.
           MOVE GR-CONTACT TO M-CONTACT.
           MOVE 1 TO LISTSUB.
       SETUP-DISP-LOOP.
           MOVE GR-MEMBER-ID(LISTSUB) TO LIST-TABLE(LISTSUB).
           IF LISTSUB < 100
              ADD 1 TO LISTSUB
              GO TO SETUP-DISP-LOOP.

           MOVE GR-HOME-BRANCH       TO M-HOMEBR.
           MOVE GR-LN-APPROVAL-LIMIT TO M-APP-LIMIT.

           MOVE MORE-BUF TO WR-BUF.
           MOVE MORE-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

           MOVE MORE-BUF2 TO WR-BUF.
           MOVE MORE-LIST2 TO WR-LIST.
           PERFORM CALL-WRFORM.

           MOVE "E  A010CONTINUE." TO VDU.
           PERFORM SCREENIO.

       MORE-INFO-CLOSE-WINDOW.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY GRSCN2-SCREEN.
           PERFORM CLOSE-WINDOW.
           MOVE "GRSCAN" TO VDU-FORM-NAME.
       MORE-INFO-EXIT.
           EXIT.


      *******************************************************
       NEXT-VALID-GR SECTION.
      *******************************************************
       NEXT-VALID-GROUP.
           PERFORM READ-GR1-FILE-NEXT.
           IF IO-FG = 9
              GO TO EXIT-VALID-GR.

           IF GR-LEVEL NOT = SPECIFIC-LEVEL
              IF SPECIFIC-LEVEL NOT = 99
                 GO TO NEXT-VALID-GROUP.
       EXIT-VALID-GR.
           EXIT.

      ********************************************
       DISPLAY-LEVELS SECTION.
      ********************************************
           MOVE 1 TO SUB.
           MOVE SPACES TO DISPLAY-BUF.
           MOVE "LV"   TO CD-TYPE.
           MOVE 0      TO CD-LV-LEVEL.
           MOVE SPACES TO CD-LV-FILLER.
           PERFORM OPEN-CD1-FILE.
           PERFORM START-CD1-FILE.
       DISPLAY-LEVELS-NEXT.
           PERFORM READ-CD1-FILE-NEXT.
           IF IO-FG = 9
              IF SUB = 1
                 MOVE "NO LEVELS ON FILE!" TO D-LINE(1)
                 MOVE SPACES TO GRSCAN-KEY
                 MOVE "1>" TO FLDSEL-STAT
                 GO TO DISPLAY-LEVELS-EXIT
              ELSE
                 GO TO DISPLAY-LEVELS-EXIT.

           IF CD-TYPE NOT = "LV"
              GO TO DISPLAY-LEVELS-EXIT.

           IF SUB = 1
              MOVE "    LEVEL  NAME" TO D-LINE(2)
              MOVE 4 TO SUB.

           MOVE CD-LV-LEVEL TO D-LEV(SUB).
           MOVE CD-DESC     TO D-DESC(SUB).
           ADD 1 TO SUB.
           GO TO DISPLAY-LEVELS-NEXT.

       DISPLAY-LEVELS-EXIT.
           MOVE DISPLAY-BUF TO WR-BUF.
           MOVE DISPLAY-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      *******************************************************
       CODE-SCAN SECTION.
      *******************************************************
           MOVE "LV"   TO CDSCAN-TYPE.
           MOVE ZEROES TO CDSCAN-LV-LEVEL.
           MOVE SPACES TO CDSCAN-LV-FILLER.

           MOVE "WI/CDSCAN" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH CDSCAN-BUF EXIT-PATHNAME.
           CANCEL FORM-PROGX.
           IF (CDSCAN-STATUS NOT = "00") OR (CDSCAN-LV-LEVEL = ZEROES)
              MOVE 9 TO IO-FG
              GO TO EXIT-CODE-SCAN.

           MOVE "SNNN010LEV." TO VDU.
           MOVE CDSCAN-LV-LEVEL TO VDUNUM0.
           PERFORM SCREENIO.
           MOVE CDSCAN-LV-LEVEL TO VDUNUM0.
           MOVE 0 TO IO-FG.

       EXIT-CODE-SCAN.
           EXIT.

      *******************************************************
       VERIFY-PASSWORD SECTION.
      *******************************************************
           MOVE " " TO PASSWDW-STAT.
           MOVE "GRSCAN" TO PASSWDW-PGM.
           MOVE "PLEASE ENTER THE PASSWORD," TO PASSWDW-PROMPT1.
           MOVE "TO VIEW GROUP INFO " TO PASSWDW-PROMPT2.
           MOVE "CL/PASSWD" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH PASSWDW-BUF.
           CANCEL FORM-PROGX.

       COPY "LIBWI/PFSWINDOWS.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/FLDSEL.CPY".
       COPY "LIBGB/DATER.CPY".


      *************************************************
       FORM-RESET SECTION.
      *************************************************
           DISPLAY GRSCAN-SCREEN UPON GRSCAN-WINDOW-HANDLE.
           MOVE GRSCAN-FORM-FIELDS TO WR-BUF.
           MOVE GRSCAN-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
       FORM-RESET-EXIT.
           EXIT.

      *************************************************
       SETUP-LINK-INFO SECTION.
      *************************************************
           MOVE GRSCAN-HELP--FILE TO HELP-LINK-FILE.
           MOVE GRSCAN-HELP--DIR  TO HELP-LINK-DIR.

      *************************************************
       VDU-CONVERT-FIELD SECTION.
      *************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBWI/GRSCAN_EVA.CPY".
              COPY "LIBWI/GRSCN2_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      *******************************************************
       MISC-ROUTINES SECTION.
      *******************************************************
       COPY "LIBGB/ACCESS.CPY".
       SEND-LEGEND.
           MOVE "S  A600WSEL:D." TO VDU.
           MOVE MESS TO VDUBUF.
           PERFORM SCREENIO.
       GET-CD-LV.
           MOVE "LV"           TO CD-TYPE.
           MOVE SPECIFIC-LEVEL TO CD-LV-LEVEL.
           MOVE SPACES         TO CD-LV-FILLER.
           PERFORM OPEN-CD1-FILE.
           PERFORM READ-CD1-FILE.
           PERFORM CLOSE-CD1-FILE.
       SEND-PHONE.
           MOVE TEL-1 TO TEL-ED1.
           MOVE TEL-2 TO TEL-ED2.
           MOVE TEL-3 TO TEL-ED3.


      **********************************
       IO-ROUTINES SECTION.
      **********************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-CD1-FILE.
           PERFORM CLOSE-GR1-FILE.
           PERFORM CLOSE-BR-FILE.
       COPY "LIBLP/LPCDGS_SQL.CPY".
       COPY "LIBGB/GBGRGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBGR1RN.CPY".
       COPY "LIBLP/LPCD1RN.CPY".
       COPY "LIBGB/GBBRRN.CPY".

       COPY "LIBGB/FERRORS.CPY".

      **********************************
       END-ROUTINE SECTION.
      **********************************
           PERFORM CLOSE-FILES.
       EXIT-PROG.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY GRSCAN-SCREEN.
           DESTROY GRSCAN-WINDOW-HANDLE.
           PERFORM CLOSE-WINDOW UNTIL WINDOWS-OPEN = 0.
       STOP-RUN.
           EXIT PROGRAM.
