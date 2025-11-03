      * COPYMEMBER: LIBGB/PASSWDW
      ******************************************************************
      *
      *     PASSWDW-BUF - USER SECURITY PASSWORD FILE LINK AREA
      *                  USED OUTSIDE OF SECURITY MODULE
      *
      *=================================================================
      * REV:
      * KEC 2015.1130 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         FIXED TO NOT USE PASSWDW-DIR ANYMORE, ADDED 1 MORE BYTE
      *         TO PASSWDW-SEQ
      * BAH 2015.1201 CHANGED FIELD PREFIX TO PASSWDW FROM PW-REC
      *
      ******************************************************************
       01  PASSWDW-BUF.
           03  PASSWDW-STAT             PIC XX.
           03  PASSWDW-KEY.
               05  PASSWDW-PGM          PIC X(10).
               05  PASSWDW-SEQ          PIC 9(3).
           03  PASSWDW-PROMPT1          PIC X(31).
           03  PASSWDW-PROMPT2          PIC X(31).
