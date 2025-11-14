# Inventário - Haskell

## RA2 - Funcional
- **Instituição de Ensino**: Pontifícia Universidade Católica do Paraná (PUCPR)
- **Disciplina**: Programação Lógica e Funcional
- **Professor Responsável**: Frank Coelho de Alcantara

## Aluna

| Nome da Aluna | Usuário GitHub |
|---------------|----------------|
| Sophia Post Ploposki | [@sosopost](https://github.com/sosopost)     |



### Comandos do Sistema

| Comando | Sintaxe | Descrição |
|---------|---------|-----------|
| `add` | `add <id> <nome> <qtd> <categoria>` | Adiciona novo item |
| `remove` | `remove <id> <qtd>` | Remove quantidade (remove item se estoque zerar) |
| `update` | `update <id> <nova_qtd>` | Atualiza quantidade (remove item se estoque zerar) |
| `listar` | `listar` | Lista todos os itens |
| `report` | `report` | Gera relatórios de erros e item mais movimentado |
| `historico` | `historico <id>` | Histórico completo do item (Add, Remove, Update) |
| `ajuda` | `ajuda` | Ajuda dos comandos |
| `sair` | `sair` | Encerra sistema |


### Teste

10 itens para teste:

```
> add 1 café 3 comida
Sucesso: ItemID: 1, Adicionado: café
> add 2 guaraná 2 bebida
Sucesso: ItemID: 2, Adicionado: guaraná
> add 3 celular 1 eletronicos
Sucesso: ItemID: 3, Adicionado: celular
> add 4 computador 4 eletronicos
Sucesso: ItemID: 4, Adicionado: computador
> add 5 livro 8 objetos
Sucesso: ItemID: 5, Adicionado: livro
> add 6 televisao 5 eletronicos
Sucesso: ItemID: 6, Adicionado: televisao
> add 7 copo 2 objetos
Sucesso: ItemID: 7, Adicionado: copo
> add 8 lasanha 1 comida
Sucesso: ItemID: 8, Adicionado: lasanha
> add 9 monitor 3 eletronicos
Sucesso: ItemID: 9, Adicionado: monitor
> add 10 blusa 9 outros
Sucesso: ItemID: 10, Adicionado: blusa
```


## Estrutura do Repositório GitHub

```
RA2_8/
├── main.hs                 # Código fonte principal
├── Inventario.dat          # Arquivo de estado
├── Auditoria.log           # Arquivo de logs
└── README.md               
```


## Compilação e Execução (Instruções)

### Online GDB
1. Acessar ([https://onlinegdb.com/_F-js8Kh-](https://www.onlinegdb.com/edit/GzldAbnq5))
2. Clicar em "Run"

### Exemplo de uso dos comandos
```
> add 1 café 3 comida
Sucesso: ItemID: 1, Adicionado: café
> add 2 guaraná 2 bebida
Sucesso: ItemID: 2, Adicionado: guaraná
> add 3 celular 1 eletronicos
Sucesso: ItemID: 3, Adicionado: celular
> add 4 computador 4 eletronicos
Sucesso: ItemID: 4, Adicionado: computador
> add 5 livro 8 objetos
Sucesso: ItemID: 5, Adicionado: livro
> add 6 televisao 5 eletronicos
Sucesso: ItemID: 6, Adicionado: televisao
> add 7 copo 2 objetos
Sucesso: ItemID: 7, Adicionado: copo
> add 8 lasanha 1 comida
Sucesso: ItemID: 8, Adicionado: lasanha
> add 9 monitor 3 eletronicos
Sucesso: ItemID: 9, Adicionado: monitor
> add 10 blusa 9 outros
Sucesso: ItemID: 10, Adicionado: blusa
> listar
--- Inventario Atual ---
ID: 1, Nome: café, Qtd: 3, Cat: comida
ID: 10, Nome: blusa, Qtd: 9, Cat: outros
ID: 2, Nome: guaraná, Qtd: 2, Cat: bebida
ID: 3, Nome: celular, Qtd: 1, Cat: eletronicos
ID: 4, Nome: computador, Qtd: 4, Cat: eletronicos
ID: 5, Nome: livro, Qtd: 8, Cat: objetos
ID: 6, Nome: televisao, Qtd: 5, Cat: eletronicos
ID: 7, Nome: copo, Qtd: 2, Cat: objetos
ID: 8, Nome: lasanha, Qtd: 1, Cat: comida
ID: 9, Nome: monitor, Qtd: 3, Cat: eletronicos

------------------------
> report
--- Relatorio de Erros ---
Nenhum erro registrado.
--- Item Mais Movimentado ---
monitor (ID: 9) -> 1 movimentações
-----------------------------
```
