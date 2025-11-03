      * COPYMEMBER: LIBLP/LOADLXAW
      ******************************************************************
      *
      * DESCRIPTION: LIBLP/LOADLXAW
      *              HOLDS LX-A AND LX-2 FIELDS FOR ADDONS.
      *
      *=================================================================
      * REV:
      * 161109 SAVED COPY LOADLXA.161109
      *        LOADLX MOVES WLXA-LN-INSTBL TO LXA-LN-INSTBL, CHANGED THIS
      *        TABLE TO BE LIKE LXA-LN-INSTBL, CHANGED COMP TO COMP-3
      *
      * KEC 2017.0509 {PR#00000} PARADATA <A30> [JAY CISZEWSKI]
      *         THE USE OF LXA-LN-INSTRM CANNOT BE FOUND ANYWHERE IN
      *         A15 & A30.  I BELIEVE IT WAS ADDED AS TO MATCH UP
      *         WITH THE USE WITHIN THE LNFILE (LN-INSTRM).
      *         I COMMENTED OUT WLXA-LN-INSTRM (MATCHING THE SAME AT
      *         LXA-LN-INSTRM IN LIBLP/LP01LX) TILL NEEDED.
      *
      ******************************************************************
       01  WLXA-WORKER.
           03  WLXA-SRCD                PIC X(2).
           03  WLXA-PMT-SCHED-CHNG  OCCURS 9 TIMES.
               05  WLXA-START-PMT       PIC 999      COMP-3.
               05  WLXA-END-PMT         PIC 999      COMP-3.
               05  WLXA-NET-CHANGE      PIC S9(6)V99  COMP.
           03  WLXA-LN-INSTBL.
               05  WLXA-LN-INSCOMP      PIC 99        COMP-3.
               05  WLXA-LN-INSOURS      PIC X.
               05  WLXA-LN-INSEFF-DATE  PIC 9(8)      COMP-3.
      *KC>     05  WLXA-LN-INSTRM   REDEFINES WLXA-LN-INSEFF-DATE
      *KC>                              PIC 9(8)      COMP-3.
               05  WLXA-LN-INSEXP-DATE  PIC 9(8)      COMP-3.
               05  WLXA-LN-INSPREM      PIC S9(7)V99  COMP-3.
               05  WLXA-LN-INSCOVR      PIC S9(7)V99  COMP-3.
               05  WLXA-LN-INSCOMM      PIC S9(7)V99  COMP-3.
           03  WLXA-LN-INS-REBATE       PIC X.
           03  WLXA-LN-INSCODES         PIC X(3).
       01  LXASUB                       PIC 9(6)      COMP-3.
       01  LXASUB2                      PIC 9(6)      COMP-3.
      *---------------------------------------------------------
      * WLXA-STATUS :          "N" = NO LX RECORDS FOUND
      *                        "1" = LXA RECORD FOUND
      *                        "2" = LXA AND LX2 RECORD FOUND
      *---------------------------------------------------------
       01  WLXA-STATUS                  PIC X         VALUE " ".
           88  WLXA-NO-RECORDS-FOUND                  VALUE "N".
           88  WLXA-RECORDS-FOUND                     VALUE "1" "2".
           88  WLXA-LXA-RECORD-FOUND                  VALUE "1".
           88  WLXA-LX2-RECORD-FOUND                  VALUE "2".
