<form name="artistForm" id="artistForm" method="post" action="/e/artistServlet">
    <label>Name:</label><input type="text" name="name" id="name" value="" /><br />
    
    <br /><br />
    <input type="hidden" name="action" id="action" value="add" />
    <input type="hidden" name="page" id="page" value="list" />
    <input type="submit" name="send" id="send" value="Send" />
</form>