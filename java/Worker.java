import java.util.regex.*;

public class Worker {

    public void doIt(String sexp){
        SexpNode root = new SexpNode();
        buildTree(sexp,root);
        TreePrinter.print(root);
    }

    public void buildTree (String sexp,SexpNode parent){
        for (int i=0; i<sexp.length(); i++){
            char c = sexp.charAt(i);
            if(c == '(' || c == ')') {
                sexp.substring(i);
                break;
            } else  parent.text += c;
        }
        Matcher m = Pattern.compile("\\((.*)\\)").matcher(sexp);
        m.find()
        System.out.println(m.group(1));
        //SexpNode node = 
        //buildTree(leftSexp);
        //buildTree(rightSexp);
    }
    
    public class SexpNode implements TreePrinter.PrintableNode {
        public String text;
        public SexpNode left;
        public SexpNode right;

        public SexpNode getLeft(){
            return this.left;
        }
        public SexpNode getRight(){
            return this.right;
        }
        public String getText(){
            return this.text;
        }
    }
}
