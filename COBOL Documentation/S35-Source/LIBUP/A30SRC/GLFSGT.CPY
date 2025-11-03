      * COPYMEMBER: LIBGL/GLFSGT
      ******************************************************************
      *
      * DESCRIPTION: FILE SELECT (FS) FOR GT-FILE - GT-REC
      *
      *=================================================================
      * REV:
      * 2016.0104 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *      REMOVED THE USE OF GT3-KEY (A15) AND MADE GT4-KEY (A15)
      *      TO NOW BE THE NEW GT3-KEY (A30).
      *
      ******************************************************************
           SELECT GT-FILE ASSIGN TO GT-PATH
                  ORGANIZATION INDEXED
                  ACCESS MODE DYNAMIC
                  LOCK MODE AUTOMATIC WITH LOCK ON RECORD
                  RECORD KEY GT1-KEY
                  ALTERNATE RECORD KEY GT2-KEY WITH DUPLICATES
                  ALTERNATE RECORD KEY GT3-KEY WITH DUPLICATES
                  FILE STATUS FILE-STAT.
