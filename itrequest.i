
/*------------------------------------------------------------------------
    File        : itrequest.i
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : Varshitha.R
    Created     : Fri May 06 12:34:25 IST 2022
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEFINE TEMP-TABLE ttUser NO-UNDO
    FIELD ttusrid     AS INTEGER  
    FIELD ttfirstName AS CHARACTER
    FIELD ttlastName  AS CHARACTER
    FIELD ttemaiid    AS CHARACTER
    FIELD ttphone     AS CHARACTER
    INDEX tp ttusrid.
    

DEFINE TEMP-TABLE ttuserTicket NO-UNDO
FIELD tusrid as INTEGER 
FIELD tusrTicketId AS INTEGER
FIELD tTicketDiscription AS CHARACTER
FIELD tcategory as CHARACTER
FIELD tticketId AS INTEGER
FIELD tstartdate AS DATE
FIELD tenddate AS DATE
FIELD tassigne AS CHARACTER
FIELD tticketStatus AS CHARACTER
INDEX tp tusrTicketId
INDEX tp1 tcategory
INDEX tp2 is primary tticketId.

    

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
