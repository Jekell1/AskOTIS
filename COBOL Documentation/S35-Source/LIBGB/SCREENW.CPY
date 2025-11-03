      ******************************************************************
      * SCREENW
      * BLM 111206 CHANGED KEY-STATUS TO MATCH A40, F7 DIDN'T WORK
      *            FROM SCAN WINDOW
      * MJD 120816 ADDED VDU-EDIT-Z? FOR CONVERTING INTEGERS FOR PROPER
      *            PRIOR TO MOVING TO SCREEN  SECTION FIELDS
      *            I.E. LN-ACCTNO, SEQ, BRANCH NUMBERS, ETC.
      * MJD 130329 CHANGED DEFAULT FOR UPPER/LOWER TO BE NEITHER.
      *            REMOVED VDU-AT-NO-UPPER AND VDU-AT-NO-LOWER  I COULD NOT
      *            DETERMINE WHAT THEY WERE USED FOOR AND DEEMED THEM REDUNDANT.
      * BLM 131014 CHANGED WR-SIZE TO WR-SIZEX, CONFLICTED WITH WR-SIZE
      *            USED FOR WRFILE
      * BLM 131120 ADDED VDU-ZERO-OR-RETURN
      * MJD 140807 ADDED VDU-NUMX THRU VDU-NUM5X AND VDUNUM THROUGH VDUNUM5X
      *            THESE ARE NEW REDEFINES OF VDU-NUM AND USED IN THE 
      *            ACCEPT STATEMENTS SO THAT WE DO NOT DISPLAY "0" AS
      *            INITIAL VALUES WHEN ENTYERING NUMERICS.
      * KEC 150526 {PR#00000} PARADATA <A30> [JAY CISZEWSKI]
      *            ADDED VDUNUM7 SINCE IT WAS NEEDED FOR TRIAD 9.
      * BLM 170309 ADDED VDU-DATE-YYYYMMDD TO RETURN ON DATE ENTRY IN
      *            ADDITION TO VDUNUM0, PD0006
      * BLM 170524 ADDED REDEF OF VDU-DATE-EDIT WITH BREAKOUT OF MM,DD,YY
      * BAH 2024.0321 REMOVED VDU-COLON-FOUND, VDU-JUNK, VDU-ERROR-FLAG,
      *               VDU-ADJ-LEN-ALP, VDU-ADJNUM, VDU-ADJDEC, VDU-ADJ-LEN
      * BAH 2024.0404 REMOVED THE FOLLOWING FROM CONAME_EXT.CPY AS EXTERNAL
      *               AND ADDED THEM IN HERE;
      *               VDU-FIELD-PROMPT VDU-WIN-L-OFFSET VDU-WIN-C-OFFSET
      *               VDU-WIN-W-MOD VDU-WIN-H-MOD VDU-WCTRL-TCLP
      *               VDU-WCTRL-FWIN VDU-WCTRL-SCAN VDU-WCTRL-EOINIT
      *               VDU-WCOLOR-TCLP VDU-WCOLOR-FWIN VDU-WCOLOR-SCAN
      *               VDU-WCOLOR-EOINIT VDU-LABEL-OFFSET
      * BAH 2024.0423 CHANGED VDU-CTRL-B-BUF FROM 50 TO 25
      **********************************************************************
       78  VDU---F1-KEY                              VALUE "10".
       78  VDU---F2-KEY                              VALUE "12".
       78  VDU---F3-KEY                              VALUE "16".
       78  VDU---F4-KEY                              VALUE "18".
       78  VDU---F5-KEY                              VALUE "1:".
       78  VDU---F6-KEY                              VALUE "1<".
       78  VDU---F7-KEY                              VALUE "1>".
       78  VDU---F8-KEY                              VALUE "1 ".
       78  VDU---F9-KEY                              VALUE "1(".
       78  VDU---F10-KEY                             VALUE "1)".
       78  VDU---F11-KEY                             VALUE "1!".
       78  VDU---F12-KEY                             VALUE "1#".
       78  VDU---DOWN-KEY                            VALUE "1D".
       78  VDU---ENTER-KEY                           VALUE "00".

       78  VDU---HOME-KEY                            VALUE "1H".
       78  VDU---LEFT-KEY                            VALUE "1L".
       78  VDU---PAGE-UP-KEY                         VALUE "1P".
       78  VDU---PAGE-DOWN-KEY                       VALUE "1N".
       78  VDU---RIGHT-KEY                           VALUE "1R".
       78  VDU---UP-KEY                              VALUE "1U".
      *=================================================================
      *01  KEY-STATUS
      *    IS SPECIAL-NAMES CRT STATUS.
      *    03  KEY-TYPE            PIC X.
      *    03  KEY-FUNCTION-CODE1  PIC 99  COMP-X.
      *    03  KEY-FUNCTION-CODE2  PIC 99  COMP-X.
       77  KEY-STATUS
           IS SPECIAL-NAMES CRT STATUS  PIC 9(4).

       01  VDU-PROGRAMMING         PIC 99   COMP.
       01  VDU-DEC-RECEIVED        PIC S99  COMP.
       01  VDU-BUFFER-LENGTH       PIC 99   COMP.
       01  VDU-ZERO-CNT            PIC 99   COMP.
       01  VDU-NUM-RECEIVED        PIC S99  COMP.
       01  VDU-SUB                 PIC 9(6) COMP.
       01  VDU-SUB2                PIC 9(6) COMP.
       01  VDU-KEY-DEST            PIC 9(2).
       01  VDU-COUNTER             PIC 9(6) COMP.
       01  VDU-GARBAGE             PIC X(61) VALUE SPACES.
       01  VDU-I                   PIC S999 COMP   VALUE +0.
       01  VDU-J                   PIC S999 COMP   VALUE +0.
       01  VDU-NEG-FLAG            PIC S9   COMP   VALUE +1.
       01  VDU-DEC-FLAG            PIC 9    COMP   VALUE 0.
       01  VDU-NM-LIST             PIC X(13).
       01  VDU-FIELD-TO-CONVERT    PIC X(13).
       01  VDU-SIZEX               PIC S9(2).
       01  VDU-POSX                PIC 9(6).
       01  VDU-SIZE                PIC 9(2).
       01  VDU-POS                 PIC 9(6).
       01  VDUPOSX REDEFINES VDU-POS.
           03  VDU-LINE            PIC 9(03).
           03  VDU-COL             PIC 9(03).
       01  VDU-DATE-CHECK.
           03  VDU-DD              PIC 99.
           03  VDU-MM              PIC 99.
           03  VDU-YY              PIC 99.
       01  VDU-DATE-TABLE.
           03  FILLER              PIC X(24) VALUE
           "312931303130313130313031".
       01  VDU-DATE-TAB            REDEFINES VDU-DATE-TABLE.
           03  VDU-DAYS            OCCURS 12 PIC 99.
       01  VDU-DATE-8-CHECK.
           03  VDU-8-DD              PIC 99.
           03  VDU-8-MM              PIC 99.
           03  VDU-8-YY              PIC 99.
       01  VDU-DATE-YYYYMMDD        PIC 9(08) VALUE ZEROES.
       01  FILLER                   REDEFINES VDU-DATE-YYYYMMDD.
           03  VDU-YYYYMMDD-YYYY    PIC 9(04).
           03  VDU-YYYYMMDD-CCYY    REDEFINES VDU-YYYYMMDD-YYYY.
               05  VDU-YYYYMMDD-CC  PIC 9(02).
               05  VDU-YYYYMMDD-YY  PIC 9(02).
           03  VDU-YYYYMMDD-MM      PIC 9(02).
           03  VDU-YYYYMMDD-DD      PIC 9(02).
       01  VDU-CC                   PIC 99 VALUE 50.
       01  VDU-SSNO-CHECK.
           03  VDU-SSNO1           PIC 999.
           03  VDU-SSNO2           PIC 99.
           03  VDU-SSNO3           PIC 9999.
       78  VDU-STOR-BUF-LEN          VALUE 80.
       01  VDU-STOR-BUF.
           03  FILLER              PIC X(VDU-STOR-BUF-LEN).
       01  VDU-ERROR-MESSAGE       PIC X(80).
       01  VDU-NAME-ERROR REDEFINES VDU-ERROR-MESSAGE.
           03  FILLER              PIC X(17).
           03  VDU-ERROR-FIELD     PIC X(13).
       01  VDU-ERROR-MESS1 REDEFINES VDU-ERROR-MESSAGE PIC X(40).
       01  VDU-BUF-AREA    REDEFINES VDU-ERROR-MESSAGE PIC X(80).
       01  VDU-BUF-TAB     REDEFINES VDU-ERROR-MESSAGE PIC X
                                                       OCCURS 80.

      *------THESE FIELDS USED FOR DISPLAY-FIELD SECTION ONLY--(VDU-DISP)-------
       01  VDU-DISP-SIZE           PIC 9(2).
       01  VDU-DISP-POS            PIC 9(6).
       01  VDU-DISP-ATTRIBUTES     PIC X(8) VALUE SPACES.
       01  VDU-DISP-ATTRIB REDEFINES VDU-DISP-ATTRIBUTES
                                   PIC X OCCURS 8.
       01  VDU-DISP-PROCESS.
           05  VDU-DISP-ACTION         PIC X.
           05  VDU-DISP-PUNC           PIC X.
           05  VDU-DISP-RESET          PIC X.
           05  VDU-DISP-CLASS          PIC X.
           05  VDU-DISP-LENGTH.
               07  VDU-DISP-NUM        PIC 99.
               07  VDU-DISP-DEC        PIC 9.
           05  VDU-DISP-FIELD         PIC X(13).
       01  VDU-DISP-BUF            PIC X(80).
      *------ABOVE FIELDS USED FOR DISPLAY-FIELD SECTION ONLY--(VDU-DISP)-------

       01  VDU-EDIT-AREA.
           03  VDU-EDIT-N.
               05  FILLER      PIC X(6).
               05  VDU-EDIT        PIC Z(17)9-.
           03  VDU-ED0             REDEFINES VDU-EDIT-N.
               05  FILLER      PIC X.
               05  VDU-EDIT0       PIC ZZZ,ZZZ,ZZZ,ZZZ,ZZZ,ZZ9-.
           03  VDU-ED1             REDEFINES VDU-EDIT-N.
               05  VDU-EDIT1       PIC ZZ,ZZZ,ZZZ,ZZZ,ZZZ,ZZ9.9-.
           03  VDU-ED2             REDEFINES VDU-EDIT-N.
               05  VDU-EDIT2       PIC Z,ZZZ,ZZZ,ZZZ,ZZZ,ZZ9.99-.
           03  VDU-ED3             REDEFINES VDU-EDIT-N.
               05  FILLER      PIC X.
               05  VDU-EDIT3       PIC ZZZ,ZZZ,ZZZ,ZZZ,ZZ9.999-.
           03  VDU-ED4             REDEFINES VDU-EDIT-N.
               05  FILLER      PIC X.
               05  VDU-EDIT4       PIC ZZ,ZZZ,ZZZ,ZZZ,ZZ9.9999-.
           03  VDU-ED5             REDEFINES VDU-EDIT-N.
               05  FILLER      PIC X.
               05  VDU-EDIT5       PIC Z,ZZZ,ZZZ,ZZZ,ZZ9.99999-.
           03  VDU-VDU-ED10            REDEFINES VDU-EDIT-N.
               05  FILLER      PIC X(17).
               05  VDU-EDIT-DATE   PIC 99/99/99.
           03  VDU-VDU-ED11            REDEFINES VDU-EDIT-N.
               05  FILLER      PIC X(15).
               05  VDU-EDIT-DATE-8 PIC 99/99/9999.
           03  FILLER                  REDEFINES VDU-EDIT-N.
               05  FILLER      PIC X(14).
               05  VDU-EDIT-SSNO   PIC 999/99/9999.
           03  VDU-VDU-EDIT-TABLE      REDEFINES VDU-EDIT-N.
               05  VDU-EDIT-TAB    PIC X OCCURS 25.

           03  VDU-EDIT-NOSIGN     PIC Z(17)9.
           03  FILLER                  REDEFINES VDU-EDIT-NOSIGN.
               05  FILLER          PIC X(10).
               05  VDU-EDIT-Z8     PIC ZZZZZZZ9.
           03  FILLER                  REDEFINES VDU-EDIT-NOSIGN.
               05  FILLER          PIC X(12).
               05  VDU-EDIT-Z6     PIC ZZZZZ9.
           03  FILLER                  REDEFINES VDU-EDIT-NOSIGN.
               05  FILLER          PIC X(13).
               05  VDU-EDIT-Z5     PIC ZZZZ9.
           03  FILLER                  REDEFINES VDU-EDIT-NOSIGN.
               05  FILLER          PIC X(14).
               05  VDU-EDIT-Z4     PIC ZZZ9.
           03  FILLER                  REDEFINES VDU-EDIT-NOSIGN.
               05  FILLER          PIC X(15).
               05  VDU-EDIT-Z3     PIC ZZ9.
           03  FILLER                  REDEFINES VDU-EDIT-NOSIGN.
               05  FILLER          PIC X(16).
               05  VDU-EDIT-Z2     PIC Z9.
           03  FILLER                  REDEFINES VDU-EDIT-NOSIGN.
               05  FILLER          PIC X(17).
               05  VDU-EDIT-Z1     PIC 9.

       78  VDU-MAX                   VALUE 102.
       01  HOLD-VDU                  PIC X(VDU-MAX).
       01  VDU                       VALUE SPACES.
           03  VDUPROCESS.
               05  VDUACTION         PIC X.
               05  VDUPUNC           PIC X.
               05  VDURESET          PIC X.
               05  VDUCLASS          PIC X.
               05  VDULENGTH.
                   07  VDUNUM        PIC 99.
                   07  VDUDEC        PIC 9.
               05  VDUFIELD         PIC X(13).
               05  VDU-FIELD-A   REDEFINES VDUFIELD.
                   07  VDU-FLD       PIC X(01)  OCCURS 13.
               05  VDU-FIELD-B   REDEFINES VDUFIELD.
                   07  VDUROW            PIC 99.
                   07  VDUCOL-ALP.
                       09  VDUCOL        PIC 9.
                   07  VDUATTR.
                       09  VDUDECIMAL    PIC X.
                       09  FILLER        PIC X(9).
           03  VDUBUFFER.
               05  VDUBUF.
                   07  VDU-NUM                     PIC 9(18).
                   07  VDUNUM0  REDEFINES VDU-NUM  PIC S9(18).
                   07  VDUNUM1  REDEFINES VDU-NUM  PIC S9(17)V9.
                   07  VDUNUM2  REDEFINES VDU-NUM  PIC S9(16)V99.
                   07  VDUNUM3  REDEFINES VDU-NUM  PIC S9(15)V999.
                   07  VDUNUM4  REDEFINES VDU-NUM  PIC S9(14)V9999.
                   07  VDUNUM5  REDEFINES VDU-NUM  PIC S9(13)V99999.
                   07  VDUNUM7  REDEFINES VDU-NUM  PIC S9(11)V9999999.
                   07  VDUNUM1NS  REDEFINES VDU-NUM  PIC 9(17)V9.
                   07  VDUNUM2NS  REDEFINES VDU-NUM  PIC 9(16)V99.
                   07  VDUNUM3NS  REDEFINES VDU-NUM  PIC 9(15)V999.
                   07  VDUNUM4NS  REDEFINES VDU-NUM  PIC 9(14)V9999.
                   07  VDUNUM5NS  REDEFINES VDU-NUM  PIC 9(13)V99999.
                   07  VDUNUM7NS  REDEFINES VDU-NUM  PIC 9(11)V9999999.
                   07  VDUNUM-TABLE       REDEFINES VDU-NUM.
                       09  VDUNUM-TAB     PIC X OCCURS 18.
                   07  VDUNUM10           REDEFINES VDU-NUM.
                       09  FILLER      PIC X(12).
                       09  VDUNUM-DATE.
                           11  VDU-DATE-1  PIC XX.
                           11  VDU-DATE-2  PIC XX.
                           11  VDU-DATE-3  PIC XX.
                   07  VDUNUM11           REDEFINES VDU-NUM.
                       09  FILLER      PIC X(10).
                       09  VDUNUM-DATE-8.
                           11  VDU-DATE-8-MM  PIC XX.
                           11  VDU-DATE-8-DD  PIC XX.
                           11  VDU-DATE-8-YY  PIC XXXX.
                   07  FILLER                REDEFINES VDU-NUM.
                       09  FILLER             PIC X(9).
                       09  VDUNUM-SSNO.
                           11  VDU-SSNO-1     PIC XXX.
                           11  VDU-SSNO-2     PIC XX.
                           11  VDU-SSNO-3     PIC XXXX.
                   07  VDU-DATE-EDIT REDEFINES VDU-NUM
                                           PIC 99/99/99B(10).
                   07  VDU-DATE-EDIT-MMDDYY REDEFINES VDU-NUM.
                       09  VDU-DATE-EDIT-MM   PIC 99/.
                       09  VDU-DATE-EDIT-DD   PIC 99/.
                       09  VDU-DATE-EDIT-YY   PIC 99.
                       09  FILLER             PIC X(10).
                   07  VDU-DATE-8-EDIT REDEFINES VDU-NUM
                                           PIC 99/99/9999B(8).
                   07  VDU-SSNO-EDIT   REDEFINES VDU-NUM
                                           PIC 999/99/9999B(7).
      *    THESE REDEFINES WERE ADDED SO THAT ACCEPT ON NUMERIC FIELDS
      *    RE DEFAULT TO A INITIAL VALUE OF " " AND NOT DISPLAY "0"
      *    WHEN ENTERRING (I.E. SEL ON LPMENU.
                   07  VDU-NUMX   REDEFINES VDU-NUM PIC  Z(18).
                   07  VDU-NUM1X  REDEFINES VDU-NUM PIC  Z(17)VZ.
                   07  VDU-NUM2X  REDEFINES VDU-NUM PIC  Z(16)VZZ.
                   07  VDU-NUM3X  REDEFINES VDU-NUM PIC  Z(15)VZZZ.
                   07  VDU-NUM4X  REDEFINES VDU-NUM PIC  Z(14)VZZZZ.
                   07  VDU-NUM5X  REDEFINES VDU-NUM PIC  Z(13)VZZZZZ.
                   07  VDUNUMX    REDEFINES VDU-NUM PIC Z(17)-.
                   07  VDUNUM1X   REDEFINES VDU-NUM PIC Z(16)VZ-.
                   07  VDUNUM2X   REDEFINES VDU-NUM PIC Z(15)VZZ-.
                   07  VDUNUM3X   REDEFINES VDU-NUM PIC Z(14)VZZZ-.
                   07  VDUNUM4X   REDEFINES VDU-NUM PIC Z(13)VZZZZ-.
                   07  VDUNUM5X   REDEFINES VDU-NUM PIC Z(12)VZZZZZ-.
      *            07  VDU-NUMX   REDEFINES VDU-NUM PIC  9(18).
      *            07  VDU-NUM1X  REDEFINES VDU-NUM PIC  9(17)V9.
      *            07  VDU-NUM2X  REDEFINES VDU-NUM PIC  9(16)V99.
      *            07  VDU-NUM3X  REDEFINES VDU-NUM PIC  9(15)V999.
      *            07  VDU-NUM4X  REDEFINES VDU-NUM PIC  9(14)V9999.
      *            07  VDU-NUM5X  REDEFINES VDU-NUM PIC  9(13)V99999.
      *            07  VDUNUMX    REDEFINES VDU-NUM PIC S9(18).
      *            07  VDUNUM1X   REDEFINES VDU-NUM PIC S9(17)V9.
      *            07  VDUNUM2X   REDEFINES VDU-NUM PIC S9(16)V99.
      *            07  VDUNUM3X   REDEFINES VDU-NUM PIC S9(15)V999.
      *            07  VDUNUM4X   REDEFINES VDU-NUM PIC S9(14)V9999.
      *            07  VDUNUM5X   REDEFINES VDU-NUM PIC S9(13)V99999.
                   07  FILLER   PIC X(62).
               05  VDUALPTAB    REDEFINES VDUBUF.
                   07  VDUTAB   PIC X     OCCURS 80.
               05  VDUALPHA     REDEFINES VDUBUF.
                   07  FILLER   PIC X(80).
           03  VDUSTATUS          PIC XX.
               88  F1-KEY             VALUE "10".
               88  F2-KEY             VALUE "12".
               88  F3-KEY             VALUE "16".
               88  F4-KEY             VALUE "18".
               88  F5-KEY             VALUE "1:".
               88  F6-KEY             VALUE "1<".
               88  F7-KEY             VALUE "1>".
               88  F8-KEY             VALUE "1 ".
               88  F9-KEY             VALUE "1(".
               88  F10-KEY            VALUE "1)".
               88  F11-KEY            VALUE "1!".
               88  F12-KEY            VALUE "1#".
               88  DOWN-KEY           VALUE "1D".
               88  ENTER-KEY          VALUE "00".
               88  HOME-KEY           VALUE "1H".
               88  LEFT-KEY           VALUE "1L".
               88  PAGE-UP-KEY        VALUE "1P".
               88  PAGE-DOWN-KEY      VALUE "1N".
               88  RIGHT-KEY          VALUE "1R".
               88  UP-KEY             VALUE "1U".
       01  VDU-TBL REDEFINES VDU   PIC X OCCURS VDU-MAX.

       01  VDU-HELP-FORM-NAM      PIC X(9).
       01  VDU-FORM-NAME          PIC X(9).
       01  VDU-INTERRUPT-FG       PIC XX    VALUE " ".
      *-------------------------------------------------
      *    "  "  - DON'T ALLOW F10 INTERRUPT
      *    "Y "  - ALLOW F10 INTERRUPT ALL OPTIONS
      *    "C1"  - ALLOW F10 INTERRUPT ALL OPTIONS
      *
       01  VDU-WIMENU-SELECTION   PIC XX    VALUE "Y".
      *-------------------------------------------------
       01  VDU-CLMENU-SELECTION   PIC XX    VALUE " ".
      *-------------------------------------------------
       01  VDU-PGMENU-SELECTION   PIC XX    VALUE "Y".
      *-------------------------------------------------
       01  VDU-F11MENU-SELECTION  PIC XX    VALUE "Y".
       01  VDU-F11MENU-PROG       PIC X(9)  VALUE " ".
      *-------------------------------------------------
       01  VDU-CTRL-N-PROG        PIC X(9)  VALUE " ".
      *-------------------------------------------------
       01  VDU-CTRL-B-PROG        PIC X(9)  VALUE " ".
       01  VDU-CTRL-B-BUF         PIC X(25).
      *-------------------------------------------------
       01  VDU-CTRL-R-PROG        PIC X(9)  VALUE " ".
      *-------------------------------------------------
       01  VDU-ZERO-OR-RETURN     PIC X     VALUE " ".
      *-------------------------------------------------

      ****************************************************
      *              USE OF VDU-ATTRIBUTES
      *
      *     PLACE ANY COMBINATION OF ATTRIBUTES INTO
      *     VDU-ATTRIBUTES, THE LAST CODES IN THE LIST
      *     OVERRIDE PRIOR CONFLICTING CODESS.
      *
      *       HIGH        HIGH-INTENSITY (DEFAULT)
      *  S -  STANDOUT    LOWER-INTENSITY
      *  R -  REVERSE     BLACK CHARS ON BRIGHT BACKGROUND
      *  F -  FLASH       BLINKING CHARACTERS
      *  U -  UNDERLINE   UNDERLINE
      *  B -  BOLD        HIGH INTENSITY
      *  D -  DIM         LOW INTENSITY
      *  E -  ECHO        (TO OVERRIDE FORM ATTRIBUTE)
      ****************************************************
       01  VDU-CONTROL                   PIC X(200).
       01  VDU-DEFAULT                   PIC X(80).
       01  VDU-INIT-FIELD                PIC X(80).
       01  VDU-HLD-CONTROL               PIC X(200).

       01  VDU-ATTRIBUTES                PIC X(8) VALUE SPACES.
       01  VDU-ATTRIB REDEFINES VDU-ATTRIBUTES
                                         PIC X OCCURS 8.

      *-----------------------------------------------------------------
      * THE KEYWORDS ALLOWED IN THE CONTROL DATA ITEM ARE LISTED AS
      * THE FOLLOWING GROUPS.
      *-----------------------------------------------------------------
       01  VDU-CONTROL-OPTIONS-TABLE.
      *  1.
           78  VDU-SET-ERASE           VALUE "ERASE".
           78  VDU-SET-NO-ERASE        VALUE "NO ERASE".
      *  2.
           78  VDU-SET-BEEP            VALUE "BEEP".
           78  VDU-SET-NO-BEEP         VALUE "NO BEEP".
      *  3.
           78  VDU-SET-HIGHLIGHT       VALUE "HIGH".
           78  VDU-SET-LOW             VALUE "LOW".
           78  VDU-SET-STANDARD        VALUE "STANDARD".
           78  VDU-SET-OFF             VALUE "OFF".
      *  4.
           78  VDU-SET-BLINKING        VALUE "BLINK".
           78  VDU-SET-NO-BLINKING     VALUE "NO BLINK".
      *  5.
           78  VDU-SET-REVERSED        VALUE "REVERSE".
           78  VDU-SET-NO-REVERSED     VALUE "NO REVERSE".
      *  6.
           78  VDU-SET-TAB             VALUE "TAB".
           78  VDU-SET-NO-TAB          VALUE "NO TAB".
      *  7.
           78  VDU-SET-NO-PROMPT       VALUE "NO PROMPT".
           03  VDU-SET-PROMPT.
               05  FILLER               PIC X(07) VALUE "PROMPT ".
               05  FILLER               PIC X(01) VALUE X'22'.
               05  VDU-SET-PROMPT-VAL   PIC X(01) VALUE SPACES.
               05  FILLER               PIC X(01) VALUE X'22'.
      *  8.
           78  VDU-SET-CONVERT         VALUE "CONVERT".
           78  VDU-SET-NO-CONVERT      VALUE "NO CONVERT".
      *  9.
           78  VDU-SET-UPDATE          VALUE "UPDATE".
           78  VDU-SET-NO-UPDATE       VALUE "NO UPDATE".
      * 10.
           78  VDU-SET-ECHO            VALUE "ECHO".
           78  VDU-SET-NO-ECHO         VALUE "SECURE".
      * 11.
           78  VDU-SET-UPPER           VALUE "UPPER".
           78  VDU-SET-NO-UPPER        VALUE "NO UPPER".
           78  VDU-SET-LOWER           VALUE "LOWER".
           78  VDU-SET-NO-LOWER        VALUE "NO LOWER".
      * 12.
           78  VDU-SET-UNDERLINED      VALUE "UNDERLINED".
           78  VDU-SET-NO-UNDERLINED   VALUE "NO UNDERLINE".
      * 13.
           78  VDU-SET-LEFT            VALUE "LEFT".
           78  VDU-SET-RIGHT           VALUE "RIGHT".
           78  VDU-SET-CENTER          VALUE "CENTERED".
           78  VDU-SET-NO-JUST         VALUE "NO JUST".
      * 14.
           78  VDU-SET-SAME            VALUE "SAME".
      * 15.
           03  VDU-SET-FCOLOR.
               05  FILLER              PIC X(07) VALUE "FCOLOR ".
               05  VDU-SET-FCOLOR-VAL  PIC 9(02).
      * 16.
           03  VDU-SET-BCOLOR.
               05  FILLER              PIC X(07) VALUE "FCOLOR ".
               05  VDU-SET-BCOLOR-VAL  PIC 9(02).
      *-----------------------------------------------------------------

       01  VDU-AT-LIST                  PIC X(16) VALUE SPACES.
       01  VDU-AT-LISTX REDEFINES VDU-AT-LIST.
           03  VDU-AT-GROUP1            PIC X.
               88  VDU-AT-ERASE                  VALUE "N".
               88  VDU-AT-NO-ERASE               VALUE "Y".
           03  VDU-AT-GROUP2            PIC X.
               88  VDU-AT-BEEP                   VALUE "Y".
               88  VDU-AT-NO-BEEP                VALUE " ".
           03  VDU-AT-GROUP3            PIC X.
               88  VDU-AT-HIGHLIGHT              VALUE " " "H".
               88  VDU-AT-BOLD                   VALUE "B".
               88  VDU-AT-LOW                    VALUE "L".
               88  VDU-AT-STANDARD               VALUE "S".
               88  VDU-AT-OFF                    VALUE "O".
           03  VDU-AT-GROUP4            PIC X.
               88  VDU-AT-BLINKING               VALUE "Y".
               88  VDU-AT-NO-BLINKING            VALUE " ".
           03  VDU-AT-GROUP5            PIC X.
               88  VDU-AT-REVERSED               VALUE "Y".
               88  VDU-AT-NO-REVERSED            VALUE " ".
           03  VDU-AT-GROUP6            PIC X.
               88  VDU-AT-TAB                    VALUE "Y".
               88  VDU-AT-NO-TAB                 VALUE "N".
           03  VDU-AT-GROUP7            PIC X.
               88  VDU-AT-PROMPT                 VALUE " ".
               88  VDU-AT-NO-PROMPT              VALUE "Y".
           03  VDU-AT-GROUP8            PIC X.
               88  VDU-AT-CONVERT                VALUE "Y".
               88  VDU-AT-NO-CONVERT             VALUE "N".
           03  VDU-AT-GROUP9            PIC X.
               88  VDU-AT-UPDATE                 VALUE "Y".
               88  VDU-AT-NO-UPDATE              VALUE "N".
           03  VDU-AT-GROUP10            PIC X.
               88  VDU-AT-ECHO                   VALUE "Y".
               88  VDU-AT-NO-ECHO                VALUE "N".
           03  VDU-AT-GROUP11            PIC X.
               88  VDU-AT-UPPER                  VALUE "U".
               88  VDU-AT-LOWER                  VALUE "L".
           03  VDU-AT-GROUP12            PIC X.
               88  VDU-AT-UNDERLINED             VALUE "Y".
               88  VDU-AT-NO-UNDERLINED          VALUE " ".
           03  VDU-AT-GROUP13            PIC X.
               88  VDU-AT-LEFT                   VALUE "L".
               88  VDU-AT-RIGHT                  VALUE "R".
               88  VDU-AT-CENTER                 VALUE "C".
               88  VDU-AT-NO-JUST                VALUE "N".
           03  VDU-AT-GROUP14            PIC X.
               88  VDU-AT-SAME                   VALUE "Y".
           03  VDU-AT-GROUP15            PIC X.
               88  VDU-AT-FCOLOR                 VALUE "Y".
           03  VDU-AT-GROUP16            PIC X.
               88  VDU-AT-BCOLOR                 VALUE "Y".
           03  VDU-AT-GROUP17            PIC X.
               88  VDU-AT-REQUIRED               VALUE "Y".

       01  VDU-POP-UP-WINDOW             PIC X(10).

       01  VDU-SEND-MESS-LINE-1     PIC X(60).
       01  VDU-SEND-MESS-LINE-2     PIC X(60).
       01  VDU-SEND-MESS-LINE-3     PIC X(60).
       01  VDU-SEND-MESS-LINE-4     PIC X(60).
       01  VDU-SEND-MESS-LINE-5     PIC X(60).
       01  VDU-SEND-MESS-LINE-6     PIC X(60).
       01  VDU-SEND-MESS-LINE-7     PIC X(60).
       01  JUNK                      PIC X(01).
       01  FLDVAL-WRFORM-WORKWERS.
           78  WR-MAX                            VALUE 2000.
           03  FLDVAL-STOR-BUF      PIC X(VDU-STOR-BUF-LEN).
           03  WR-LIST              PIC X(WR-MAX).
           03  WR-BUF               PIC X(WR-MAX).
           03  WRSUB                PIC 9(6).
           03  WR-STATX             PIC X(02).
           03  WR-STAT REDEFINES WR-STATX
                                    PIC 9(02).
           03  WR-START             PIC 9(6).
           03  WR-SIZEX             PIC 9(6).
           03  WR-PTR               PIC 9(04).
           03  WR-LIST-SIZE         PIC 9(04).
           03  WR-ELEMENT           PIC X(14).
           03  WR-LIST-EMPTY        PIC X(01) VALUE "N".
               88  WR-NO-MORE-ELEMENTS        VALUE "Y".

       01  GETFM-WORKERS.
           78  GETFM-MAX                           VALUE WR-MAX.
           03  GETFM-ND-INIT-BUF    PIC X(GETFM-MAX).
           03  GETFM-ND-INIT-LIST   PIC X(GETFM-MAX).
       01  NDIS-LINK-BUF.
           03  NDIS-ACTION          PIC X(01).
           03  NDIS-BUF             PIC X(15).
           03  NDIS-LIST            PIC X(80).
      *  MJD PLAY WORKERS TESTING RJUS SHIFT
       01  VDU-ACTUAL-LENGTH        PIC S9(2).
       01  VDU-NEW-START            PIC S9(2).
       01  VDU-SUBA                  PIC 9(2).
       01  VDU-ATRX.
               03  VDU-ATR-HOLD  PIC X(01).
               03  VDU-ATR-RJUS  PIC X(01).
               03  VDU-ATR-BOLD  PIC X(01).
               03  VDU-ATR-AUTO  PIC X(01).
               03  VDU-ATR-ZFIL  PIC X(01).
               03  VDU-ATR-VALU  PIC X(01).
      * "Y" IN 'MUST' ALLOWS LOWERCASE 
               03  VDU-ATR-MUST  PIC X(01). 
               03  VDU-ATR-CONS  PIC X(01).
               03  VDU-ATR-TOTL  PIC X(01).
               03  VDU-ATR-PERM  PIC X(01).
               03  VDU-ATR-ECHO  PIC X(01).
       01  VDU-FORM-ATTRX REDEFINES VDU-ATRX.
               03  VDU-FORM-ATTR OCCURS 11 PIC X.

       01 VDU-TEMP-BUF                PIC X(80).
       01 VDU-CP                      PIC 9(06).
       01 VDU-BP                      PIC 9(06).

       01 VDU-FIELD-PROMPT            PIC X     VALUE "U".
          88  VDU-FIELD-PROMPT-REV    VALUE "R".
          88  VDU-FIELD-PROMPT-ULINE  VALUE "U".
       01 VDU-WIN-L-OFFSET            PIC 99     VALUE 1.
       01 VDU-WIN-C-OFFSET            PIC 99     VALUE 1.
       01 VDU-WIN-W-MOD               PIC 99     VALUE 0.
       01 VDU-WIN-H-MOD               PIC 99     VALUE 0.
       01 VDU-WCTRL-TCLP              PIC 9(3)   VALUE 13.
       01 VDU-WCTRL-FWIN              PIC 9(3)   VALUE 13.
       01 VDU-WCTRL-SCAN              PIC 9(3)   VALUE 13.
       01 VDU-WCTRL-EOINIT            PIC 9(3)   VALUE 13.
       01 VDU-WCOLOR-TCLP             PIC 9(3)   VALUE 80.
       01 VDU-WCOLOR-FWIN             PIC 9(3)   VALUE 80.
       01 VDU-WCOLOR-SCAN             PIC 9(3)   VALUE 80.
       01 VDU-WCOLOR-EOINIT           PIC 9(3)   VALUE 80.
       01 VDU-LABEL-OFFSET            PIC 9(3)   VALUE 0.

      ******************************************************************
      *
      * DO NOT ADD FIELDS AFTER THIS LINE. THIS MUST BE THE LAST LINE
      * OF THIS COPY MEMBER BECAUSE IT IS REDEFINED BY EACH PROGRAM
      *
      ******************************************************************
       01  VDU-ALP-FIELD                 PIC X(13).
