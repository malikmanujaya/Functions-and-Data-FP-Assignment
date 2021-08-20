object MyApp{
  def main(args:Array[String]): Unit ={
    val R1 = new Rational(3,4)
    val x = new Rational(3,4)
    val y = new Rational(5,8)
    val z = new Rational(2,7)
    val account1 = new Account("Account",1,10000)
    val account2 = new Account("Account",2,3000)
    val account3 = new Account("Account",3,-1000)
    val account4 = new Account("Account",4,1500)
    val account5 = new Account("Account",5,6000)
    val account6 = new Account("Account",6,900)
    val account7 = new Account("Account",7,-200)
    val account8 = new Account("Account",8,500)


    var bank:List[Account] = List(account1,account2,account3,account4,account5,account6,account7,account8)


    println("Q1\n-----------------------------")
    println("Negation of (3,4) is : "+R1.negation)

    println("\nQ2\n-----------------------------")
    println("(3,4)-(5,8)-(2,7) : "+(x-y-z))

    println("\nQ3\n-----------------------------")
    println("Balance of Account1 : "+account1.balance)
    println("Balance of Account2 : "+account2.balance)
    println("Transfered Rs.1000  from Account1 to Account2 ")
    account1.transfer(account2,1000)
    println("Updated balance of Account1: "+account1.balance)
    println("Updated balance of Account2 : "+account2.balance)


    println("\nQ4\n-----------------------------")
    println("Question Number 4.1\n List of Accounts with negative balances : \n")
    balance(bank).foreach(i => println(i))
    println("\nQuestion Number 4.2\nsum of all account balances : "+Total(bank)+"\n")
    println("Question Number 4.3\nFinal balances of the account after applying the interest : "+Total(interest(bank)))
  }

  val balance=(b:List[Account])=>b.filter(x=>x.balance<0)
  val Total=(b:List[Account])=>b.reduce((x,y)=>new Account("",0,x.balance+y.balance)).balance
  val interest=(b:List[Account])=>b.map(x=>if(x.balance>=0) new Account(x.account,x.account_number,x.balance+x.balance*0.005) else new Account(x.account,x.account_number,x.balance+x.account_number*0.001) )

}

class Rational(n:Int,d:Int){
  def numer = n
  def denom = d

  def negation = new Rational(-this.numer,this.denom)

  def -(r:Rational) = new Rational(this.numer*r.denom-r.numer*this.denom,this.denom*r.denom)
  override def toString = numer+"/"+denom
}

class Account(Account:String,n:Int,b:Double){
  val account : String = Account
  val account_number : Int = n
  var balance: Double = b

  def transfer(a:Account,b:Double) = {
    this.balance = this.balance - b
    a.balance = a.balance + b
  }

  override def toString = "["+account+" "+account_number+" = "+balance+"]"
}
