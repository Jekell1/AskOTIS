      * COPYMEMBER: LIBLP/LPBP.CMD
      **********************************************************************
      *
      *   LPBP - COMMANDS
      *
      *=================================================================
      * REV:
      * JTG 010607 ADDED LIST5-CMD
      *            GP-TRUE-NEXT-BATCH           (MERC PR#243/251)
      * JTG 021016 ADDED LIST-REF-CMD.
      *            ADDED NEW CODE FILE TYPE 'BR' - BATCH PAYMENT REFERENCE CODES
      *            BATCH PAYMENT REVISIONS                          REGACC #0006
      * JTG 021104 ADDED LIST2-REF-CMD. AND DEV-NULL ' 2>DEV/NULL '
      *            BATCH PAYMENT REVISIONS                          REGACC #0006
      * BAH 140519 MOVED FROM _CMD, USE C$TOLOWER WHEN NEEDED!!
      *
      * KEC 2017.0912 {PR#00000} PARADATA <A30> [JAY CISZEWSKI]
      *          COPIED LIBLP/LPBP.CPY TO THE SV AS A .NO THEN RENAME IT
      *          TO BE LIBLP/LPBP_CMD.CPY THEN FIXED IT FOR WHAT NEEDED
      *          TO BE LOWERCASE.
      *
      **********************************************************************
       01  DEV-NULL       PIC X(12) VALUE " 2>/dev/null".
       01  CUT-CMD        PIC X(11) VALUE "cut -c17-80".
       01  CUT-LOCAL-CMD  PIC X(11) VALUE "cut -c24-80".

      * ALSO USED IN SP/BPBKBR TO ENSURE THAT NO B??0000000 EXIST
      * BEFORE CREATING NEW ONES
       01  LIST-PATH      PIC X(14) VALUE "/usr/tmp/batch".

      * CALLED FROM BPBKBR
      *  "/$REL/SH/ec /$REL/SH/bplist.sh ".
       01  LIST-CMD.
           03  FILLER               PIC X    VALUE "/".
           03  LIST-REL             PIC X(7).
           03  FILLER               PIC X(4) VALUE "/SH/".
           03  LIST-EC              PIC X(3) VALUE "ec ".
           03  FILLER               PIC X    VALUE "/".
           03  LIST-REL2            PIC X(7).
           03  FILLER               PIC X(4) VALUE "/SH/".
           03  LIST-SHELL           PIC X(10) VALUE "bplist.sh ".
           03  LIST-DATA            PIC X(18).


      *  "/$REL/SH/ec /$REL/SH/bplist_ref.sh ".
       01  LIST-REF-CMD.
           03  FILLER               PIC X    VALUE "/".
           03  LIST-REF-REL         PIC X(7).
           03  FILLER               PIC X(4) VALUE "/SH/".
           03  LIST-REF-EC          PIC X(3) VALUE "ec ".
           03  FILLER               PIC X    VALUE "/".
           03  LIST-REF-REL2        PIC X(7).
           03  FILLER               PIC X(4) VALUE "/SH/".
           03  LIST-REF-SHELL       PIC X(14) VALUE "bplist_ref.sh ".
           03  LIST-REF-REFCD       PIC X(5).
           03  FILLER               PIC X     VALUE " ".
           03  LIST-REF-DATA        PIC X(11).
           03  FILLER               PIC X     VALUE " ".
           03  LIST-REF-DATE        PIC X(4).

      * "/$REL/SH/ec /$REL/SH/bplist2_ref.sh ".
       01  LIST2-REF-CMD.
           03  FILLER               PIC X    VALUE "/".
           03  LIST2-REF-REL        PIC X(7).
           03  FILLER               PIC X(4) VALUE "/SH/".
           03  LIST2-REF-EC         PIC X(3) VALUE "ec ".
           03  FILLER               PIC X    VALUE "/".
           03  LIST2-REF-REL2       PIC X(7).
           03  FILLER               PIC X(4) VALUE "/SH/".
           03  LIST2-REF-SHELL      PIC X(15) VALUE "bplist2_ref.sh ".
           03  LIST2-REF-BRNO       PIC 9(4).
           03  FILLER               PIC X.
           03  LIST2-REF-DATE       PIC 9(4).
           03  FILLER               PIC X     VALUE " ".
           03  LIST2-REF-BATCH      PIC 9(3).


      * USED IN LP/LONPL0 & SP/AUTOCL TO SEE THAT ALL BATCHES HAVE BEEN
      * UPDATED
       01  BPEOD-PATH.
           03  FILLER        PIC X(14) VALUE
               "/usr/tmp/bpeod".
           03  BPEOD-PATH-BR PIC 9(4).

      *  "/$REL/SH/ec /$REL/SH/bpeod.sh ".
       01  BPEOD-CMD.
           03  FILLER            PIC X    VALUE "/".
           03  BPEOD-REL         PIC X(7).
           03  FILLER            PIC X(4) VALUE "/SH/".
           03  BPEOD-EC          PIC X(3) VALUE "ec ".
           03  FILLER            PIC X    VALUE "/".
           03  BPEOD-REL2        PIC X(7).
           03  FILLER            PIC X(4) VALUE "/SH/".
           03  BPEOD-SHELL       PIC X(9) VALUE "bpeod.sh ".
           03  BPEOD-BR          PIC 9(4).
           03  FILLER            PIC XX    VALUE SPACES.
           03  BPEOD-MO          PIC 99.
           03  FILLER            PIC XX    VALUE SPACES.
           03  BPEOD-DA          PIC 99.
           03  FILLER            PIC XX    VALUE SPACES.
           03  BPEOD-DATA        PIC X(18).


      *  "/$REL/SH/ec /$REL/SH/bplist2.sh ".
      *01  LIST2-CMD.
      *    03  FILLER               PIC X    VALUE "/".
      *    03  LIST2-REF-REL        PIC X(7).
      *    03  FILLER               PIC X(4) VALUE "/SH/".
      *    03  LIST2-REF-EC         PIC X(3) VALUE "ec ".
      *    03  FILLER               PIC X    VALUE "/".
      *    03  LIST2-REF-REL2       PIC X(7).
      *    03  FILLER               PIC X(4) VALUE "/SH/".
      *    03  LIST2-REF-SHELL      PIC X(11) VALUE "bplist2.sh ".
      *    03  LIST2-DATE  PIC 9(4).

      *  "/$REL/SH/ec /$REL/SH/bplist5.sh ".
      *01  LIST5-CMD.
      *    03  FILLER               PIC X    VALUE "/".
      *    03  LIST5-REF-REL        PIC X(7).
      *    03  FILLER               PIC X(4) VALUE "/SH/".
      *    03  LIST5-REF-EC         PIC X(3) VALUE "ec ".
      *    03  FILLER               PIC X    VALUE "/".
      *    03  LIST5-REF-REL2       PIC X(7).
      *    03  FILLER               PIC X(4) VALUE "/SH/".
      *    03  LIST5-REF-SHELL      PIC X(11) VALUE "bplist5.sh ".
      *    03  LIST5-DATE  PIC 9(4).
