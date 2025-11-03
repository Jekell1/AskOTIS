      * COPYMEMBER: LIBGB/MKTEMPW
      **********************************************************
      *  LIMIT COMPLETE PATH TO 34 BYTES
      *  MKTEMP-STAR MAY BE USED IN
      *  REMOVING INDEXED TEMPS (FILE*)
      *  CREATED WITH MKTEMP-DEFAULT
      *  NOTE:
      *       DO NOT CHANGE THE CASE OF MKTEMP-DEFAULT
      *  REV:
      *  BLM 081020 ADDED DATA-MKTEMP-BUF FOR CREATING TMP FILES
      *             SOMEWHERE OTHER THAN /usr/tmp.  ON BIG
      *             SYSTEMS TMP FILES ON RAID IMPROVES I/O SPEED
      *             ON TMP FILES DRAMATICALLY.  THIS OPTION
      *             WILL ONLY BE IMPLEMENTED IN CERTAIN PROGRAMS
      *             FOR CERTAIN CUSTOMERS; TMP FILES WILL REMAIN
      *             IN /usr/tmp if /usr/data/r1/WK DIRECTORY
      *             DOESN'T EXIST
      *  BLM 120327 ADDED MKTEMP-TMP, PEOPLES PR# 769
      *  JKC 130430 FIXED FOR FULL PATHS IN A30
      *  BLM 140417 ADDED MKTEMP-SAVE-FORM-PATH
      *  JKC 230710 MOVED THIS TO BE MKTEMP_CMD.CPY SINCE THIS
      *             HAS LOWERCASE CHARACTERS;
      *             REMOVED THE FOLLOWING SINCE NOT USED:
      *               DATA-MKTEMP-BUF
      *               MKTEMP-TMP
      *               MKTEMP-SAVE-FORM-PATH
      *
      **********************************************************
       01  MKTEMP-DEFAULT             PIC X(15) VALUE "/usr/tmp/XXXXXX".
       01  MKTEMP-BUF.
           03  FILLER                 PIC X(15).
           03  MKTEMP-STAR            PIC X.
           03  FILLER                 PIC X(19).
       01  FILLER                     REDEFINES MKTEMP-BUF.
           03  FILLER                 PIC X(09).
           03  MKTEMP-NAME            PIC X(26).
       01  MKTEMP-BUF-RED             REDEFINES MKTEMP-BUF.
           03  FILLER                 PIC X(9).
           03  PROCESS-ID-BUF         PIC X(7).
           03  FILLER                 PIC X(19).
       01  MKTEMP-SAVE-FORM-PATH      PIC X(22).
