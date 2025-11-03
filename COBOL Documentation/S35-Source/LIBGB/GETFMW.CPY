      * COPYMEMBER: LIBGB/GETFMW
      *-----------------------------------------------------------------------
      *    NOTE: A MODIFIED COPY OF THIS EXISTS IN
      *          GB/CONAME.C AND GB/CONAM2.C
      *          SO IF YOU ALTER FORM-PATH CHANGE
      *          CONAME.C & CONAM2.C
      * REV:
      *  050719 ADDED FORM-SPANISH FOR WORLD SPANISH SCREENS, WORLD PR# 414
      *  051013 ADDED REDEFINITION OF FORM-PATHNAME FOR TEST IN DECLARTIVES
      *  101124 ADDED GP-REC-FOR-SEC, GETFM WILL DO GETENV ON GP-REC TO
      *         CHECK FLAG FOR NEW SECURITY, PR# NEWSECURITY
      *  121107 ADDED EXIT-PATHNAME
      *  BLM 130219 ADDED 03'S TO SAVE-FORM-PATH SO WE CAN USE EXIT-PATH
      *             EG LP/LPRPMU.  SECURITY PASSES BACK STAT IN FORM-OBJ,
      *             SO NEED TO PRESERVE THIS PART OF THE PATH
      *  BLM 130415 ADDED REDEFS OF FORM-PATH & FORM-PATHNAME NEEDED IN
      *             SECURITY
      *  BAH 140512 REMOVED FORM-SPANISH, ADDED AN EXT-SPANISH-FORMS TO CONAME
      *  BAH 151007 COMMENTED OUT GP-NEW-SECURITY AND GP-REC-FOR-SEC
      *  BLM 161220 REMOVE TEST-FORM-1ST4 REDEF IN PATHNAME
      *  KEC 170112 ADDED ANOTHER REDEFINE ON SAVE-FORM-PATH FOR
      *             HAVING SAVE-FORM-PROGX SO THAT WE CAN PROPERLY
      *             CANCEL A PROGRAM AFTER CALLING TO IT.
      *  BAH 180814 RENAMED CHKSEC-FIELDS TO CHKSEC-FIELDS
      *                     CHKSEC-STAT TO CHKSEC-STAT
      *                     SAVE-FORM-PATH TO CHKSEC-FORM-PATH
      *             REMOVED SAVE-FORM-PROGX
      *  BAH 2024.0311 REMOVED FORM-STAT
      *-----------------------------------------------------------------------
       01  FORM-PATH.
           03  FILLER         PIC X.
           03  FORM-REL       PIC X(7).
           03  FILLER REDEFINES FORM-REL.
               05  FILLER     PIC X(4).
               05  LOGUID-REL PIC X(3).
           03  FILLER         PIC X.
           03  FORM-OBJ       PIC XX.
           03  FILLER         PIC X.
           03  FORM-NAM.
               05  FORM-DIRX  PIC XX.
               05  FILLER     PIC X.
               05  FORM-PROGX PIC X(6).
           03  FILLER         PIC X.

       01  FORM-PROG          PIC X(6) VALUE " ".

       01  CHKSEC-FIELDS.
           03  CHKSEC-FORM-PATH    PIC X(22).
           03  CHKSEC-STAT         PIC X(02).

       LINKAGE SECTION.

       01  FORM-PATHNAME      PIC X(22).
       01  EXIT-PATHNAME.
           03  FILLER         PIC X(12).
           03  EXIT-PATH      PIC X(9).
           03  FILLER         PIC X.
