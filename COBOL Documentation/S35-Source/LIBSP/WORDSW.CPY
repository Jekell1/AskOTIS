      * COPYMEMBER: LIBSP/WORDSW
      ******************************************************************
      *
      * IDENTIFICATION:  LIBSP/WORDSW
      *
      * DESCRIPTION   :  WORDS ROUTINE (WORKERS)
      *
      *                  USED TO FIND WORDS FROM A INPUT FIELD.
      *
      *=================================================================
      * REV:
      * KEC 2006.0411 {PR#00000} PARADATA <A15> [KEN CISZEWSKI]
      *               ***NEW***
      *
      * KEC 2006.0626 {PR#00000} PARADATA <A15> [KEN CISZEWSKI]
      *               INCREASED WORDS---MAX FROM 100 TO 200.
      *
      ******************************************************************
       78  WORDS---MAX                               VALUE 200.

      *-----------------------------------------------------------------
       77  WORDS-COUNT                 PIC 9(03)     VALUE ZEROES.
       77  WORDS-ELE                   PIC 9(03)     VALUE ZEROES.
       77  WORDS-PREV-BYTE             PIC X(01)     VALUE SPACES.
       77  WORDS-S1                    PIC 9(03)     VALUE ZEROES.
       77  WORDS-S2                    PIC 9(03)     VALUE ZEROES.

      *=================================================================
       01  WORDS-INPUT                  PIC X(WORDS---MAX) VALUE SPACES.
       01  FILLER                       REDEFINES WORDS-INPUT.
           03  WORDS-INPUT-ELE          PIC X(01)    OCCURS WORDS---MAX.

      *=================================================================
       01  WORDS-OUTPUT-WORKERS.
           03  WORDS-OUTPUT-TABLE       OCCURS WORDS---MAX.
               05  WORDS-OUTPUT-COUNT   PIC 9(03).
               05  WORDS-OUTPUT         PIC X(WORDS---MAX).
               05  FILLER               REDEFINES WORDS-OUTPUT.
                   07  WORDS-OUTPUT-ELE PIC X(01) OCCURS WORDS---MAX.

      *=================================================================
       01  WORDS-WRAP-LINES               PIC 9(03)     VALUE ZEROES.

       01  WORDS-WRAP-LINE-WORKERS.
           03  WORDS-WRAP-LINE-TABLE      OCCURS WORDS---MAX.
               05  WORDS-WRAP-LINE-SIZE   PIC 9(03).
               05  WORDS-WRAP-LINE        PIC X(WORDS---MAX).
