/*
Exercício 1: Manipulação Básica de Listas
Problema: Dada uma lista de números, filtre apenas os números pares e calcule a soma.
*/

val numeros = List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
val numerosPares = numeros.filter(_ % 2 == 0)
val soma = numerosPares.sum
println(s"Números pares: $numerosPares")
println(s"Soma: $soma")

/*
Exercício 2: Transformação de Strings
Problema: Normalize uma lista de nomes removendo espaços extras e convertendo para formato adequado.
*/


val nomes = List("  joao silva  ", "MARIA santos", "  Pedro   OLIVEIRA  ")
val nomesNormalizados = nomes.map(nome => 
  nome.trim.toLowerCase.split("\\s+").map(_.capitalize).mkString(" ")
)
println(nomesNormalizados)

/*
Exercício 3: Agrupamento de Dados
Problema: Agrupe vendas por categoria e calcule o total por categoria.
*/
case class Venda(produto: String, categoria: String, valor: Double)
val vendas = List(
  Venda("Notebook", "Eletrônicos", 2500.0),
  Venda("Mouse", "Eletrônicos", 50.0),
  Venda("Livro", "Educação", 30.0),
  Venda("Curso Online", "Educação", 200.0)
)

val vendasPorCategoria = vendas
  .groupBy(_.categoria)
  .mapValues(_.map(_.valor).sum)

vendasPorCategoria.foreach {case (categoria, total) =>
  println(s"$categoria: $total")
}

/*
Exercício 4: Leitura e Processamento de CSV Simples
Problema: Simule a leitura de um CSV e calcule estatísticas básicas.
*/
case class Funcionario(id: Int, nome: String, idade: Int, salario: Double)

val csvData = """id,nome,idade,salario
1,João,25,3000
2,Maria,30,4500
3,Pedro,35,5200
4,Ana,28,3800"""

val linhas = csvData.split("\n").toList
val funcionarios = linhas.tail.map { linha =>
  val campos = linha.split(",")
  Funcionario(campos(0).toInt, campos(1), campos(2).1toInt, campos(3).toDouble)
}

val salarioMedio = funcionarios.map(_.salario).sum / funcionarios.length
val idadeMedia = funcionarios.map(_.idade).sum / funcionarios.length
val salarioMaximo = funcionarios.map(_.salario).max

println(s"Salário médio:  $salarioMedio")
println(s"Idade média: $idadeMedia")
println(s"Maior salário:  $salarioMaximo")

/*
Exercício 5: Validação de Dados
Problema: Valide emails e filtre apenas os válidos de uma lista.
*/

val emails = List("joao@email.com", "maria.invalid", "pedro@test.co", "ana@", "@domain.com")

def validarEmail(email: String): Boolean = {
  val emailRegex = "^[A-Za-z0-9+_.-]+@[A-Za-z0-9.-]+\\.[A-Za-z]{2,}$".r
  emailRegex.matches(email)
}

val emailsValidos = emails.filter(validarEmail)
val emailsInvalidos = emails.filterNot(validarEmail)

println(s"Emails válidos: $emailsValidos")
println(s"Emails inválidos: $emailsInvalidos")

/*
Exercício 6: Transformação de Dados com Option
Problema: Processe dados que podem estar ausentes usando Option.
*/
case class Usuario(id: Int, nome: String, email: Option[String], idade: Option[Int])
val usuarios = List(
  Usuario(1, "João", Some("joao@email.com"), Some(25)),
  Usuario(2, "Maria", None, Some(30)),
  Usuario(3, "Pedro", Some("pedro@email.com"), None)
)

// Usuários com email
val usuariosComEmail = usuarios.filter(_.email.isDefined)
println(s"Usuários com email: ${usuariosComEmail.length}")

// Idade média dos usuários que têm idade informada
val idadeMedia = usuarios
  .flatMap(_.idade)
  .sum.toDouble / usuarios.count(_.idade.isDefined)
println(s"Idade média: $idadeMedia")

// Relatório completo
usuarios.foreach { usuario =>
  val emailInfo = usuario.email.getOrElse("Não informado")
  val idadeInfo = usuario.idade.map(_.toString).getOrElse("Não informado")
  println(s"${usuario.nome}: email=$emailInfo, idade=$idadeInfo")
}

/*
Exercício 7: Contagem e Frequência
Problema: Analise logs de acesso e encontre os IPs mais frequentes.
*/
val logsAcesso = List(
  "192.168.1.1", "10.0.0.1", "192.168.1.1", "172.16.0.1",
  "192.168.1.1", "10.0.0.1", "192.168.1.2", "192.168.1.1"
)

val frequenciaIPs = logsAcesso
  .groupBy(identity)
  .mapValues(_.length)
  .toList
  .sortBy(-_._2)

println("IPs mais frequentes:")
frequenciaIPs.foreach { case (ip, count) =>
  println(s"$ip: $count acessos")
}

// Top 3 IPs
val top3IPs = frequenciaIPs.take(3)
println(s"\nTop 3 IPs: $top3IPs")

/*
Exercício 8: Junção de Dados
Problema: Faça join entre duas listas simulando tabelas relacionais.
*/
case class Cliente(id: Int, nome: String)
case class Pedido(id: Int, clienteId: Int, produto: String, valor: Double)
case class PedidoCompleto(pedidoId: Int, clienteNome: String, produto: String, valor: Double)

val clientes = List(
  Cliente(1, "João"), Cliente(2, "Maria"), Cliente(3, "Pedro")
)
val pedidos = List(
  Pedido(101, 1, "Notebook", 2500), Pedido(102, 2, "Mouse", 50),
  Pedido(103, 1, "Teclado", 150), Pedido(104, 4, "Monitor", 800)
)

// Inner Join
val pedidosCompletos = for {
  pedido <- pedidos
  cliente <- clientes if cliente.id == pedido.clienteId
} yield PedidoCompleto(pedido.id, cliente.nome, pedido.produto, pedido.valor)

println("Pedidos com clientes:")
pedidosCompletos.foreach(println)

// Left Join (todos os pedidos, mesmo sem cliente)
val todosOsPedidos = pedidos.map { pedido =>
  val clienteNome = clientes.find(_.id == pedido.clienteId).map(_.nome).getOrElse("Cliente não encontrado")
  PedidoCompleto(pedido.id, clienteNome, pedido.produto, pedido.valor)
}

println("\nTodos os pedidos:")
todosOsPedidos.foreach(println)

/*
Exercício 9: Tratamento de Exceções em Processamento de Dados
Problema: Processe dados que podem gerar exceções de forma segura.
*/
import scala.util.{Try, Success, Failure}

val dadosNumericos = List("123", "456", "abc", "789", "xyz", "0")

// Conversão segura
val resultados = dadosNumericos.map { dado =>
  Try(dado.toInt) match {
    case Success(numero) => Some(numero)
    case Failure(exception) => 
      println(s"Erro ao converter '$dado': ${exception.getMessage}")
      None
  }
}

val numerosValidos = resultados.flatten
val numeroInvalidos = dadosNumericos.length - numerosValidos.length

println(s"Números válidos: $numerosValidos")
println(s"Soma dos números válidos: ${numerosValidos.sum}")
println(s"Quantidade de dados inválidos: $numeroInvalidos")

// Usando collect para uma abordagem mais funcional
val numerosValidosCollect = dadosNumericos.collect {
  case s if Try(s.toInt).isSuccess => s.toInt
}
println(s"Usando collect: $numerosValidosCollect")

/*
Exercício 10: Agregação Temporal
Problema: Agrupe vendas por mês e calcule métricas mensais.
*/
import java.time.LocalDate
case class VendaDiaria(data: LocalDate, produto: String, quantidade: Int, valor: Double)

val vendas = List(
  VendaDiaria(LocalDate.of(2024, 1, 15), "Produto A", 10, 100.0),
  VendaDiaria(LocalDate.of(2024, 1, 20), "Produto B", 5, 250.0),
  VendaDiaria(LocalDate.of(2024, 2, 10), "Produto A", 8, 80.0),
  VendaDiaria(LocalDate.of(2024, 2, 25), "Produto C", 12, 360.0)
)

// Agrupar por ano-mês
val vendasMensais = vendas
  .groupBy(v => (v.data.getYear, v.data.getMonth))
  .mapValues { vendasDoMes =>
    val totalQuantidade = vendasDoMes.map(_.quantidade).sum
    val totalValor = vendasDoMes.map(_.valor).sum
    val produtosUnicos = vendasDoMes.map(_.produto).distinct.length
    (totalQuantidade, totalValor, produtosUnicos)
  }

println("Vendas mensais:")
vendasMensais.foreach { case ((ano, mes), (qtd, valor, produtos)) =>
  println(s"$mes/$ano: $qtd unidades,  $valor, $produtos produtos diferentes")
}

/*
Exercício 11: Parser de Log Complexo
Problema: Parse logs de servidor web com diferentes formatos.
*/
import scala.util.matching.Regex
import java.time.LocalDateTime
import java.time.format.DateTimeFormatter

case class LogEntry(
  ip: String,
  user: Option[String],
  timestamp: String,
  method: String,
  path: String,
  protocol: String,
  status: Int,
  size: Int
)

val logs = List(
  "127.0.0.1 - - [10/Oct/2000:13:55:36 -0700] \"GET /apache_pb.gif HTTP/1.0\" 200 2326",
  "127.0.0.1 - - [10/Oct/2000:13:55:37 -0700] \"POST /login HTTP/1.1\" 401 0",
  "192.168.1.1 - user [10/Oct/2000:13:55:38 -0700] \"GET /home HTTP/1.1\" 200 1024"
)

val logPattern: Regex = """(\S+) \S+ (\S+) \[([^\]]+)\] "(\S+) (\S+) (\S+)" (\d+) (\d+)""".r

def parseLog(logLine: String): Option[LogEntry] = {
  logLine match {
    case logPattern(ip, user, timestamp, method, path, protocol, status, size) =>
      Some(LogEntry(
        ip = ip,
        user = if (user == "-") None else Some(user),
        timestamp = timestamp,
        method = method,
        path = path,
        protocol = protocol,
        status = status.toInt,
        size = size.toInt
      ))
    case _ => None
  }
}

val parsedLogs = logs.flatMap(parseLog)

// Análises
val statusCounts = parsedLogs.groupBy(_.status).mapValues(_.length)
val methodCounts = parsedLogs.groupBy(_.method).mapValues(_.length)
val totalTraffic = parsedLogs.map(_.size).sum

println("Status codes:")
statusCounts.foreach { case (status, count) => println(s"  $status: $count") }

println("\nMethods:")
methodCounts.foreach { case (method, count) => println(s"  $method: $count") }

println(s"\nTotal traffic: $totalTraffic bytes")

/*
Exercício 12: ETL Pipeline Básico
Problema: Implemente um pipeline ETL para transformar dados de vendas.
*/
import scala.util.parsing.json._
import java.time.LocalDate
import java.time.format.DateTimeFormatter

case class VendaBruta(data: LocalDate, produto: String, quantidade: Int, precoUnitario: Double)
case class Desconto(produto: String, desconto: Double)
case class VendaProcessada(
  data: LocalDate, 
  produto: String, 
  quantidade: Int, 
  precoUnitario: Double,
  desconto: Double,
  valorBruto: Double,
  valorLiquido: Double
)

// Extract
def extrairVendas(csv: String): List[VendaBruta] = {
  val linhas = csv.split("\n").toList
  linhas.tail.map { linha =>
    val campos = linha.split(",")
    VendaBruta(
      data = LocalDate.parse(campos(0)),
      produto = campos(1),
      quantidade = campos(2).toInt,
      precoUnitario = campos(3).toDouble
    )
  }
}

def extrairDescontos(json: String): Map[String, Double] = {
  // Simulação simples de parse JSON
  Map(
    "Produto A" -> 0.1,
    "Produto B" -> 0.15
  )
}

// Transform
def transformarVendas(vendas: List[VendaBruta], descontos: Map[String, Double]): List[VendaProcessada] = {
  vendas.map { venda =>
    val desconto = descontos.getOrElse(venda.produto, 0.0)
    val valorBruto = venda.quantidade * venda.precoUnitario
    val valorLiquido = valorBruto * (1 - desconto)
    
    VendaProcessada(
      data = venda.data,
      produto = venda.produto,
      quantidade = venda.quantidade,
      precoUnitario = venda.precoUnitario,
      desconto = desconto,
      valorBruto = valorBruto,
      valorLiquido = valorLiquido
    )
  }
}

// Load (simula salvamento)
def carregarDados(vendas: List[VendaProcessada]): Unit = {
  println("=== DADOS PROCESSADOS ===")
  vendas.foreach(println)
  
  val resumo = vendas.groupBy(_.produto).mapValues { vendasProduto =>
    val totalQuantidade = vendasProduto.map(_.quantidade).sum
    val totalBruto = vendasProduto.map(_.valorBruto).sum  
    val totalLiquido = vendasProduto.map(_.valorLiquido).sum
    (totalQuantidade, totalBruto, totalLiquido)
  }
  
  println("\n=== RESUMO POR PRODUTO ===")
  resumo.foreach { case (produto, (qtd, bruto, liquido)) =>
    println(s"$produto: $qtd unidades,  $bruto (bruto),  $liquido (líquido)")
  }
}