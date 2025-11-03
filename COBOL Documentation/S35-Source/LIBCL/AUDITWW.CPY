
      * COPYMEMBER: LIBCL/AUDITWW
      *********************************************************************
      *   AUDITW
      *                 (CL)
      *   DESCRIPTION:  WORKERS FOR AUDITW.C  AUDIT CHANGES LOG
      *                 WRITES GB/AUFILE
      *
      *     AUDITW-CHANGE-TYPE   "D" DELETE
      *                          "A" ADD
      *                          "C" CHANGE
      *
      * REV:
      *  20220207 BAH NEW AUDIT/CHANGE CALLED PROGRAM, #1542
      *********************************************************************
       01  AUDITW-WORKERS.
           03  AUDITW-FILE-NAME      PIC X(7).
           03  AUDITW-CHANGE-TYPE    PIC X.
           03  AUDITW-ENTER-TICKET   PIC X      VALUE "Y".
           03  AUDITW-HOLD-TICKET    PIC X(20)  VALUE SPACES.
           03  AUDITW-PROGRAM        PIC X(8)   VALUE SPACES.
           03  AUDITW-ORIG-REC       PIC X(6400).
           03  AUDITW-CHG-REC        PIC X(6400).

