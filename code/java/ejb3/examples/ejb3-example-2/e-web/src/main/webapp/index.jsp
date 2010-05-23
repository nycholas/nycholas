<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core" %>
<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">
<html xmlns="http://www.w3.org/1999/xhtml">
<head>
    <meta http-equiv="Content-Type" content="text/html; charset=utf-8" />
    <title>Simple Example EJB3 - 2</title>
</head>

<body>
    <form name="artistForm" id="artistForm" method="post" action="/e/artistServlet">
        <label>Name:</label><input type="text" name="name" id="name" value="" /><br />
        
        <br /><br />
        <input type="hidden" name="choice" id="choice" value="add" />
        <input type="submit" name="send" id="send" value="Send" />
    </form>
</body>
</html>
