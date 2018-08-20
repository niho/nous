# Nous: A Phenomenological Programming Language

Nous is a declarative language for writing formal language grammars. Nous is therefor a kind of meta language, useful for complex data processing. Using Nous you can construct abstract grammars that can be used for validation or classification. The syntax and concepts are inspired by LISP, Elm and BNF. The runtime architecture (VM) is partly inspired by Erlang (distribution and fault tolerance).

## Rules
The Nous language is a formal way to specify a set of *rules* for processing (specific) input into (generic) output. I.e. transforming something particular into something abstract (and back again). A Nous *grammar* can contain any number of rules and each rule can have many different definitions. Rules are arranged hierarchically and can reference other rules in their definitions. A rule is identified by a *symbol*, which is the name of the rule, followed by the expression `= {}`.

	Rule = {Case1, Case2, Case3, …}

## Cases
A rule consists of one or multiple *cases*, contained in the curly braces. The list of cases is separated by commas. A case can either be another rule, a transformation of another rule, a sequence or an input.

## Inputs
An *input* is how particulars gets into a rule. An input can be one-dimensional or multi-dimensional. A one-dimensional input is any lower case word or character. A multi-dimensional input is a comma separated list of lower case words or characters enclosed in parentheses.

	Rule = {a, (a, b)}

You never have to worry about the total number of inputs for a rule. Nous will automatically infer the number of inputs needed for a rule to execute, taking into account the inputs of sub-rules (if any).

## Transformations
A *transformer* is a Nous grammar rule with a single input and a single output. It is used as a macro to rearrange or modify a grammatical rule as it is used. A transformation is identified in usage by the syntax `A B` or `A b`. Where the first term is the transformer and the second term is the rule (or input) that is being transformed. A transformer is defined using the `->` operator, with the left hand side describing the input and the right hand side the output. An important distinction to keep in mind is that a transformation does not directly modify the actual values that are processed by a rule. It only modifies the grammatical structure of the rule itself.

	Rule = a b -> b a
	
## Example 1. Literals

	Dog = { "poodle", "labrador" }
	
	Cat = { "🐈", "🐱", "😺" }
	
	Prime = { 2, 3, 5, 7, 11, 13 }

## Example 2. Logic gates

	Not = a b -> b a

	True = {Not False}
	False = {Not True}

	And a b = {
		True = {(a, a)},
		False = {(a, b), (b, a), (b, b)}
	}

	Or a b = {
		True = {(a, a), (a, b), (b, a)},
		False = {(b, b)}
	}

	Xor a b = {
		True = {(a, b), (b, a)},
		False = {(a, a), (b, b)}
	}


## Example 3. Classification

	Mammal a = {
		Dog = {a},
		Cat = {a}
	}
	
	Number a = {
		Prime = {a}
	}
	
	Main a = {
		True = { Mammal },
		False = { Number }
	}
