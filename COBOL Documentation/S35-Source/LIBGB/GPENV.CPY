      * COPYMEMBER: LIBGB/GPENV
      ******************************************************************
      *                    (GPENV)
      *   GLOBAL PARAMETER RECORD ENVIRONMENT
      *
      *   COPY : GPENVW
      ******************************************************************
      *   DO *NOT* ADD ANY TYPE OF COMP FIELDS TO GP-REC (KEY 1);
      *   SET ENVIRONMENT (GB/SETENV) WILL NOT WORK WITH IT.
      *   LOOK AT THE SHARED DRIVE FOR 'COBOL_SET_ENV_COMP.DOCX 
      *   FOR MORE INFORMATION.                     BH & KC 2017-1004
      ******************************************************************
      * REV:
      ******************************************************************
       GET-GPENV SECTION.
           ACCEPT GP-REC FROM ENVIRONMENT "GPENV".
