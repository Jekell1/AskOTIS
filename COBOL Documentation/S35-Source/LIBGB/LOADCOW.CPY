      * COPYMEMBER: LIBGB/LOADCOW
      ******************************************************************
      *
      * LOADCOW:  WORKERS FOR SUB-PROGRAM LOAD AND EXEC
      *
      *=================================================================
      * REV:
      * BLV 061098 ADD FIELDS FOR CHANGES TO H_LOGUID.C
      * BLV 061198 ADD LOGUID-INTERBR TO LOG BR# FOR INTER
      *             BRANCH PMTS AND BULK TRANSFER UPDATE FOR H_LOGUID.C
      *     050719 ADDED FORM-SPANISH FOR WORLD SPANISH SCREENS, WORLD PR# 414
      *     101124 ADDED GP-REC-FOR-SEC, GETFM WILL DO GETENV ON GP-REC TO
      *            CHECK FLAG FOR NEW SECURITY, PR# NEWSECURITY
      * BAH 180814 RENAMED CHKSEC-FIELDS TO CHKSEC-FIELDS
      *                    CHKSEC-STAT   TO CHKSEC-STAT
      *                    SAVE-FORM-PATH TO CHKSEC-FORM-PATH
      *
      * JKC 2021-0112 ADDED CALLED-FORM-NAM FOR USE IN LIBGB/LOADCO.CPY;
      * BAH 20220802 COMMENTED OUT LOGUID
      * BAH 20240311 REMOVED FORM-STAT, FORM-SPANISH
      *
      ******************************************************************
       01  CALLED-FORM-NAM        PIC X(9).

       01  LOADCO-OPT             VALUE " ".
           03  LOADCO-OP1         PIC X.
           03  FILLER             PIC X(14).
           03  LOADCO-ARG         PIC X(9).

       01  FORM-PATH.
           03  FILLER             PIC X.
           03  FORM-REL           PIC X(7).
           03  FILLER REDEFINES FORM-REL.
               05  FILLER         PIC X(4).
               05  LOGUID-REL     PIC X(3).
           03  FILLER             PIC X.
           03  FORM-OBJ           PIC XX.
           03  FILLER             PIC X.
           03  FORM-NAM.
               05  FORM-DIRX      PIC XX.
               05  FILLER         PIC X.
               05  FORM-PROGX     PIC X(6).
           03  FILLER             PIC X.
       01  FORM-PROG              PIC X(9).

       01  CHKSEC-FIELDS.
           03  CHKSEC-FORM-PATH   PIC X(22).
           03  CHKSEC-STAT        PIC XX.
