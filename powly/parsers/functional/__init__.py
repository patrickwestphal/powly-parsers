from pyparsing import alphas
from pyparsing import alphanums
from pyparsing import Forward
from pyparsing import Literal
from pyparsing import nums
from pyparsing import OneOrMore
from pyparsing import Optional
from pyparsing import Word
from pyparsing import ZeroOrMore
from pyparsing import lineEnd
from pyparsing import restOfLine
from pyparsing import White

from powly.parsers import POWLyParser


class FunctionalSyntaxParser(POWLyParser):
    """
    Definition from
    https://www.w3.org/TR/owl2-syntax/#Canonical_Parsing_of_OWL_2_Ontologies:

    ontologyDocument := { prefixDeclaration } Ontology
    prefixDeclaration := 'Prefix' '(' prefixName '=' fullIRI ')'
    Ontology :=
        'Ontology' '(' [ ontologyIRI [ versionIRI ] ]
           directlyImportsDocuments
           ontologyAnnotations
           axioms
        ')'
    ontologyIRI := IRI
    versionIRI := IRI
    directlyImportsDocuments := { 'Import' '(' IRI ')' }
    axioms := { Axiom }
    """
    # helper literals
    open_paren = Literal('(')
    close_paren = Literal(')')
    open_angle = Literal('<')
    close_angle = Literal('>')
    equals = Literal('=')
    colon = Literal(':')
    dot = Literal('.')
    dash = Literal('-')
    underscore = Literal('_')
    slash = Literal('/')
    hash = Literal('#')
    question_mark = Literal('?')
    double_circum = Literal('^^')
    at = Literal('@')
    percent = Literal('%')

    # syntax definition
    # PN_CHARS_BASE ::= [A-Z] | [a-z] | [#x00C0-#x00D6] | [#x00D8-#x00F6] |
    #   [#x00F8-#x02FF] | [#x0370-#x037D] | [#x037F-#x1FFF] | [#x200C-#x200D] |
    #   [#x2070-#x218F] | [#x2C00-#x2FEF] | [#x3001-#xD7FF] | [#xF900-#xFDCF] |
    #   [#xFDF0-#xFFFD] | [#x10000-#xEFFFF]
    pn_chars_base = Word(alphas)  # FIXME: hex ranges above missing!

    # PN_CHARS_U ::= PN_CHARS_BASE | '_'
    pn_chars_u = OneOrMore(pn_chars_base | underscore)

    # PN_CHARS 	  ::= PN_CHARS_U | '-' | [0-9] | #x00B7 | [#x0300-#x036F] |
    #   [#x203F-#x2040]
    pn_chars = pn_chars_u | dash | nums  # FIXME: hex ranges above missing

    # ( PN_CHARS_U | [0-9] ) ((PN_CHARS|'.')* PN_CHARS)?
    pn_local = (pn_chars_u | nums) + \
        Optional(ZeroOrMore(pn_chars | dot) + pn_chars)

    pn_prefix = pn_chars_base + Optional(ZeroOrMore(pn_chars | dot) + pn_chars)
    prefix_name = Optional(pn_prefix) + colon
    # FIXME: this is just to get started; not a correct expression for IRIs!
    full_iri = open_angle + OneOrMore(Word(alphanums) | colon | dash |
                                      slash | dot | hash | question_mark |
                                      underscore | percent) + \
               close_angle
    prfx_decl = prefix_name + equals + full_iri
    prefix_declaration = Word('Prefix') + open_paren + prfx_decl + \
        close_paren + lineEnd
    iri = full_iri  # | abbreviated_iri  # FIXME
    ontology_iri = iri
    version_iri = iri
    directly_imports_documents = Optional(Word('Import') + open_paren + iri +
                                          close_paren)
    annotation = Word('')  # FIXME
    # ontologyAnnotations := { Annotation }
    ontology_annotations = ZeroOrMore(annotation)

    axiom_annotations = ZeroOrMore(annotation)

    non_negative_integer = nums

    datatype = iri
    constraining_facet = iri

    data_range = Forward()
    # DataIntersectionOf :=
    #           'DataIntersectionOf' '(' DataRange DataRange { DataRange } ')'
    data_intersection_of = \
        Word('DataIntersectionOf') + open_paren + data_range + data_range + ZeroOrMore(data_range) + close_paren

    # DataUnionOf := 'DataUnionOf' '(' DataRange DataRange { DataRange } ')'
    data_union_of = \
        Word('DataUnionOf') + open_paren + data_range + data_range + \
        ZeroOrMore(data_range) + close_paren

    # DataComplementOf := 'DataComplementOf' '(' DataRange ')'
    data_complement_of = \
        Word('DataComplementOf') + open_paren + data_range + close_paren

    quoted_string = alphanums  # FIXME
    lexical_form = quoted_string

    # typedLiteral := lexicalForm '^^' Datatype
    typed_literal = lexical_form + double_circum + datatype

    # stringLiteralNoLanguage := quotedString
    string_literal_no_language = quoted_string

    language_tag = at + alphas  # FIXME
    # stringLiteralWithLanguage := quotedString languageTag
    string_literal_with_language = quoted_string + language_tag

    # Literal := typedLiteral | stringLiteralNoLanguage |
    #               stringLiteralWithLanguage
    literal = typed_literal | string_literal_no_language | \
        string_literal_with_language

    restriction_value = literal

    # DataOneOf := 'DataOneOf' '(' Literal { Literal } ')'
    data_one_of = \
        Word('DataOneOf') + open_paren + literal + ZeroOrMore(literal) + \
        close_paren

    # DatatypeRestriction :=
    #    'DatatypeRestriction' '(' Datatype constrainingFacet
    #              restrictionValue { constrainingFacet restrictionValue } ')'
    datatype_restriction = \
        Word('DatatypeRestriction') + open_paren + datatype + \
        constraining_facet + restriction_value + \
        ZeroOrMore(constraining_facet + restriction_value) + close_paren

    # DataRange := Datatype | DataIntersectionOf | DataUnionOf |
    #   DataComplementOf | DataOneOf | DatatypeRestriction
    data_range << datatype | data_intersection_of | data_union_of | \
        data_complement_of | data_one_of | datatype_restriction

    class_ = iri
    obj_prop = iri
    data_prop = iri
    ann_prop = iri
    named_indiv = iri
    node_id = Literal('_:') + pn_local
    anonymous_individual = node_id
    individual = named_indiv | anonymous_individual
    # InverseObjectProperty := 'ObjectInverseOf' '(' ObjectProperty ')'
    inverse_obj_prop =\
        Word('ObjectInverseOf') + open_paren + obj_prop + close_paren

    # Entity :=
    #   'Class' '(' Class ')' |
    #   'Datatype' '(' Datatype ')' |
    #   'ObjectProperty' '(' ObjectProperty ')' |
    #   'DataProperty' '(' DataProperty ')' |
    #   'AnnotationProperty' '(' AnnotationProperty ')' |
    #   'NamedIndividual' '(' NamedIndividual ')'
    entity = \
        (Word('Class') + open_paren + class_ + close_paren) | \
        (Word('Datatype') + open_paren + datatype + close_paren) | \
        (Word('ObjectProperty') + open_paren + obj_prop + close_paren) | \
        (Word('DataProperty') + open_paren + data_prop + close_paren) | \
        (Word('AnnotationProperty') + open_paren + ann_prop + close_paren) | \
        (Word('NamedIndividual') + open_paren + named_indiv + close_paren)
    # Declaration := 'Declaration' '(' axiomAnnotations Entity ')'
    declaration = Word('Declaration') + open_paren + axiom_annotations + \
                  entity + close_paren

    class_expression = Forward()
    # ObjectIntersectionOf :=
    #   'ObjectIntersectionOf' '(' ClassExpression ClassExpression
    #                              { ClassExpression } ')'
    object_intersection_of = \
        Word('ObjectIntersectionOf') + open_paren + class_expression + \
        class_expression + ZeroOrMore(class_expression) + close_paren

    # ObjectUnionOf := 'ObjectUnionOf' '(' ClassExpression ClassExpression
    #                                      { ClassExpression } ')'
    object_union_of = \
        Word('ObjectUnionOf') + open_paren + class_expression + \
        class_expression + ZeroOrMore(class_expression) + close_paren

    # ObjectComplementOf := 'ObjectComplementOf' '(' ClassExpression ')'
    object_complement_of = \
        Word('ObjectComplementOf') + open_paren + class_expression + close_paren

    # ObjectOneOf := 'ObjectOneOf' '(' Individual { Individual }')'
    object_one_of = Word('ObjectOneOf') + open_paren + individual + \
        ZeroOrMore(individual) + close_paren

    # ObjectPropertyExpression := ObjectProperty | InverseObjectProperty
    object_property_expression = obj_prop | inverse_obj_prop

    # DataPropertyExpression := DataProperty
    data_property_expression = data_prop

    # ObjectSomeValuesFrom :=
    #   'ObjectSomeValuesFrom' '(' ObjectPropertyExpression ClassExpression ')'
    object_some_values_from = \
        Word('ObjectSomeValuesFrom') + open_paren + \
        object_property_expression + class_expression + close_paren

    # ObjectAllValuesFrom :=
    #   'ObjectAllValuesFrom' '(' ObjectPropertyExpression ClassExpression ')'
    object_all_values_from = \
        Word('ObjectAllValuesFrom') + open_paren + \
        object_property_expression + class_expression + close_paren

    # ObjectHasValue := 'ObjectHasValue' '(' ObjectPropertyExpression
    #                                        Individual ')'
    object_has_value = \
        Word('ObjectHasValue') + open_paren + object_property_expression + \
        individual + close_paren

    # ObjectHasSelf := 'ObjectHasSelf' '(' ObjectPropertyExpression ')'
    object_has_self = \
        Word('ObjectHasSelf') + open_paren + object_property_expression + \
        close_paren

    # ObjectMinCardinality :=
    #   'ObjectMinCardinality' '(' nonNegativeInteger ObjectPropertyExpression
    #                              [ ClassExpression ] ')'
    object_min_cardinality = \
        Word('ObjectMinCardinality') + open_paren + non_negative_integer + \
        object_property_expression + Optional(class_expression)

    # ObjectMaxCardinality :=
    #   'ObjectMaxCardinality' '(' nonNegativeInteger
    #                          ObjectPropertyExpression [ ClassExpression ] ')'
    object_max_cardinality = \
        Word('ObjectMaxCardinality') + open_paren + non_negative_integer + \
        object_property_expression + Optional(class_expression) + close_paren

    # ObjectExactCardinality :=
    #   'ObjectExactCardinality' '(' nonNegativeInteger
    #                          ObjectPropertyExpression [ ClassExpression ] ')'
    object_exact_cardinality = \
        Word('ObjectExactCardinality') + open_paren + non_negative_integer + \
        object_property_expression + Optional(class_expression)

    # DataSomeValuesFrom :=
    #   'DataSomeValuesFrom' '(' DataPropertyExpression
    #                            { DataPropertyExpression } DataRange ')'
    data_some_values_from = \
        Word('DataSomeValuesFrom') + open_paren + data_property_expression + \
        ZeroOrMore(data_property_expression) + data_range + close_paren

    # DataAllValuesFrom :=
    #   'DataAllValuesFrom' '(' DataPropertyExpression
    #                           { DataPropertyExpression } DataRange ')'
    data_all_values_from = \
        Word('DataAllValuesFrom') + open_paren + data_property_expression + \
        ZeroOrMore(data_property_expression) + data_range + close_paren

    # DataHasValue := 'DataHasValue' '(' DataPropertyExpression Literal ')'
    data_has_value = \
        Word('DataHasValue') + open_paren + data_property_expression + \
        literal + close_paren

    # DataMinCardinality :=
    #       'DataMinCardinality' '(' nonNegativeInteger DataPropertyExpression
    #                                [ DataRange ] ')'
    data_min_cardinality = \
        Word('DataMinCardinality') + open_paren + non_negative_integer + \
        data_property_expression + Optional(data_range)

    # DataMaxCardinality :=
    #    'DataMaxCardinality' '(' nonNegativeInteger DataPropertyExpression
    #                             [ DataRange ] ')'
    data_max_cardinality = \
        Word('DataMaxCardinality') + open_paren + non_negative_integer + \
        data_property_expression + Optional(data_range) + close_paren

    # DataExactCardinality :=
    #    'DataExactCardinality' '(' nonNegativeInteger DataPropertyExpression
    #                               [ DataRange ] ')'
    data_exact_cardinality = \
        Word('DataExactCardinality') + open_paren + non_negative_integer + \
        data_property_expression + Optional(data_range) + close_paren

    # ClassExpression :=
    #     Class | ObjectIntersectionOf | ObjectUnionOf | ObjectComplementOf |
    #     ObjectOneOf | ObjectSomeValuesFrom | ObjectAllValuesFrom |
    #     ObjectHasValue | ObjectHasSelf | ObjectMinCardinality |
    #     ObjectMaxCardinality | ObjectExactCardinality | DataSomeValuesFrom |
    #     DataAllValuesFrom | DataHasValue | DataMinCardinality |
    #     DataMaxCardinality | DataExactCardinality
    class_expression << \
        class_ | object_intersection_of | object_union_of | \
        object_complement_of | object_one_of | object_some_values_from | \
        object_all_values_from | object_has_value | object_has_self | \
        object_min_cardinality | object_max_cardinality | \
        object_exact_cardinality | data_some_values_from | \
        data_all_values_from | data_has_value | data_min_cardinality | \
        data_max_cardinality | data_exact_cardinality

    sub_class_expression = class_expression
    super_class_expression = class_expression

    # SubClassOf := 'SubClassOf' '(' axiomAnnotations subClassExpression
    #                                superClassExpression ')'
    sub_class_of = Word('SubClassOf') + open_paren + axiom_annotations + \
                   sub_class_expression + super_class_expression + close_paren

    # EquivalentClasses :=
    #       'EquivalentClasses' '(' axiomAnnotations ClassExpression
    #                               ClassExpression { ClassExpression } ')'
    equivalent_classes = \
        Word('EquivalentClasses') + open_paren + axiom_annotations + \
        class_expression + class_expression + ZeroOrMore(class_expression) + \
        close_paren

    # DisjointClasses :=
    #       'DisjointClasses' '(' axiomAnnotations ClassExpression
    #                             ClassExpression { ClassExpression } ')'
    disjoint_classes = \
        Word('DisjointClasses') + open_paren + axiom_annotations + \
        class_expression + class_expression + ZeroOrMore(class_expression) + \
        close_paren

    # disjointClassExpressions :=
    #       ClassExpression ClassExpression { ClassExpression }
    disjoint_class_expressions = \
        class_expression + class_expression + ZeroOrMore(class_expression)

    # DisjointUnion :=
    #    'DisjointUnion' '(' axiomAnnotations Class disjointClassExpressions ')'
    disjoint_union = \
        Word('DisjointUnion') + open_paren + axiom_annotations + class_ + \
        disjoint_class_expressions + close_paren

    # ClassAxiom := SubClassOf | EquivalentClasses | DisjointClasses |
    #   DisjointUnion
    class_axiom = sub_class_of | equivalent_classes | disjoint_classes | \
        disjoint_union

    # propertyExpressionChain :=
    #   'ObjectPropertyChain' '(' ObjectPropertyExpression
    #                             ObjectPropertyExpression
    #                             { ObjectPropertyExpression } ')'
    property_expression_chain = \
        Word('ObjectPropertyChain') + open_paren + \
        object_property_expression + object_property_expression + \
        ZeroOrMore(object_property_expression) + close_paren

    # subObjectPropertyExpression :=
    #       ObjectPropertyExpression | propertyExpressionChain
    sub_object_property_expression = \
        object_property_expression | property_expression_chain

    super_object_property_expression = object_property_expression

    # SubObjectPropertyOf :=
    #   'SubObjectPropertyOf' '(' axiomAnnotations subObjectPropertyExpression
    #                             superObjectPropertyExpression ')'
    sub_object_property_of = \
        Word('SubObjectPropertyOf') + open_paren + axiom_annotations + \
        sub_object_property_expression + super_object_property_expression + \
        close_paren

    # EquivalentObjectProperties :=
    #    'EquivalentObjectProperties' '(' axiomAnnotations
    #                        ObjectPropertyExpression ObjectPropertyExpression
    #                        { ObjectPropertyExpression } ')'
    equivalent_object_properties = \
        Word('EquivalentObjectProperties') + open_paren + axiom_annotations + \
        object_property_expression + object_property_expression + \
        ZeroOrMore(object_property_expression) + close_paren

    # DisjointObjectProperties :=
    #   'DisjointObjectProperties' '(' axiomAnnotations
    #   ObjectPropertyExpression ObjectPropertyExpression
    #   { ObjectPropertyExpression } ')'
    disjoint_object_properties = \
        Word('DisjointObjectProperties') + open_paren + axiom_annotations + \
        object_property_expression + object_property_expression + \
        ZeroOrMore(object_property_expression) + close_paren

    # InverseObjectProperties :=
    #   'InverseObjectProperties' '(' axiomAnnotations ObjectPropertyExpression
    #                                 ObjectPropertyExpression ')'
    inverse_object_properties = \
        Word('InverseObjectProperties') + open_paren + axiom_annotations + \
        object_property_expression + object_property_expression + close_paren

    # ObjectPropertyDomain :=
    #   'ObjectPropertyDomain' '(' axiomAnnotations ObjectPropertyExpression
    #   ClassExpression ')'
    object_property_domain = \
        Word('ObjectPropertyDomain') + open_paren + axiom_annotations + \
        object_property_expression + class_expression + close_paren

    # ObjectPropertyRange :=
    #   'ObjectPropertyRange' '(' axiomAnnotations ObjectPropertyExpression
    #                             ClassExpression ')'
    object_property_range = \
        Word('ObjectPropertyRange') + open_paren + axiom_annotations + \
        object_property_expression + class_expression + close_paren

    # FunctionalObjectProperty :=
    #       'FunctionalObjectProperty' '(' axiomAnnotations
    #                                      ObjectPropertyExpression ')'
    functional_object_property = \
        Word('FunctionalObjectProperty') + open_paren + axiom_annotations + \
        object_property_expression + close_paren

    # InverseFunctionalObjectProperty :=
    #   'InverseFunctionalObjectProperty' '(' axiomAnnotations
    #                                         ObjectPropertyExpression ')'
    inverse_functional_object_property = \
        Word('InverseFunctionalObjectProperty') + open_paren + \
        axiom_annotations + object_property_expression + close_paren

    # ReflexiveObjectProperty :=
    #   'ReflexiveObjectProperty' '(' axiomAnnotations
    #                                 ObjectPropertyExpression ')'
    reflexive_object_property = \
        Word('ReflexiveObjectProperty') + open_paren + axiom_annotations + \
        object_property_expression + close_paren

    # IrreflexiveObjectProperty :=
    #   'IrreflexiveObjectProperty' '(' axiomAnnotations
    #                                   ObjectPropertyExpression ')'
    irreflexive_object_property = \
        Word('IrreflexiveObjectProperty') + open_paren + axiom_annotations + \
        object_property_expression + close_paren

    # SymmetricObjectProperty :=
    #   'SymmetricObjectProperty' '(' axiomAnnotations
    #                                 ObjectPropertyExpression ')'
    symmetric_object_property = \
        Word('SymmetricObjectProperty') + open_paren + axiom_annotations + \
        object_property_expression + close_paren

    # AsymmetricObjectProperty :=
    #   'AsymmetricObjectProperty' '(' axiomAnnotations
    #                                  ObjectPropertyExpression ')'
    asymmetric_object_property = \
        Word('AsymmetricObjectProperty') + open_paren + axiom_annotations + \
        object_property_expression + close_paren

    # TransitiveObjectProperty :=
    #   'TransitiveObjectProperty' '(' axiomAnnotations
    #                                  ObjectPropertyExpression ')'
    transitive_object_property = \
        Word('TransitiveObjectProperty') + open_paren + axiom_annotations + \
        object_property_expression + close_paren

    # ObjectPropertyAxiom :=
    #   SubObjectPropertyOf | EquivalentObjectProperties |
    #   DisjointObjectProperties | InverseObjectProperties |
    #   ObjectPropertyDomain | ObjectPropertyRange |
    #   FunctionalObjectProperty | InverseFunctionalObjectProperty |
    #   ReflexiveObjectProperty | IrreflexiveObjectProperty |
    #   SymmetricObjectProperty | AsymmetricObjectProperty |
    #   TransitiveObjectProperty
    object_property_axiom = sub_object_property_of | \
        equivalent_object_properties | disjoint_object_properties | \
        inverse_object_properties | object_property_domain | \
        object_property_range | functional_object_property | \
        inverse_functional_object_property | reflexive_object_property | \
        irreflexive_object_property | symmetric_object_property | \
        asymmetric_object_property | transitive_object_property

    sub_data_property_expression = data_property_expression
    super_data_property_expression = data_property_expression

    # SubDataPropertyOf :=
    #   'SubDataPropertyOf' '(' axiomAnnotations subDataPropertyExpression
    #                           superDataPropertyExpression ')'
    sub_data_property_of = \
        Word('SubDataPropertyOf') + open_paren + axiom_annotations + \
        sub_data_property_expression + super_data_property_expression + \
        close_paren

    # EquivalentDataProperties :=
    #   'EquivalentDataProperties' '(' axiomAnnotations DataPropertyExpression
    #                   DataPropertyExpression { DataPropertyExpression } ')'
    equivalent_data_properties = \
        Word('EquivalentDataProperties') + open_paren + axiom_annotations + \
        data_property_expression + data_property_expression + \
        ZeroOrMore(data_property_expression) + close_paren

    # DisjointDataProperties :=
    #   'DisjointDataProperties' '(' axiomAnnotations DataPropertyExpression
    #                   DataPropertyExpression { DataPropertyExpression } ')'
    disjoint_data_properties = \
        Word('DisjointDataProperties') + open_paren + axiom_annotations + \
        data_property_expression + data_property_expression + \
        ZeroOrMore(data_property_expression) + close_paren

    # DataPropertyDomain :=
    #   'DataPropertyDomain' '(' axiomAnnotations DataPropertyExpression
    #                            ClassExpression ')'
    data_property_domain = \
        Word('DataPropertyDomain') + open_paren + axiom_annotations + \
        data_property_expression + class_expression + close_paren

    # DataPropertyRange :=
    #   'DataPropertyRange' '(' axiomAnnotations
    #                           DataPropertyExpression DataRange ')'
    data_property_range = \
        Word('DataPropertyRange') + open_paren + axiom_annotations + \
        data_property_expression + data_range + close_paren

    # FunctionalDataProperty :=
    #   'FunctionalDataProperty' '(' axiomAnnotations DataPropertyExpression ')'
    functional_data_property = \
        Word('FunctionalDataProperty') + open_paren + axiom_annotations + \
        data_property_expression + close_paren

    # DataPropertyAxiom :=
    #   SubDataPropertyOf | EquivalentDataProperties | DisjointDataProperties |
    #   DataPropertyDomain | DataPropertyRange | FunctionalDataProperty
    data_property_axiom = sub_data_property_of | equivalent_data_properties | \
        disjoint_data_properties | data_property_domain | \
        data_property_range | functional_data_property

    # DatatypeDefinition :=
    #   'DatatypeDefinition' '(' axiomAnnotations Datatype DataRange ')'
    datatype_definition = \
        Word('DatatypeDefinition') + open_paren + axiom_annotations + \
        datatype + data_range + close_paren

    # HasKey :=
    #   'HasKey' '(' axiomAnnotations
    #       ClassExpression '(' { ObjectPropertyExpression } ')'
    #       '(' { DataPropertyExpression } ')' ')'
    has_key = \
        Word('HasKey') + open_paren + axiom_annotations + class_expression + \
        open_paren + ZeroOrMore(object_property_expression) + close_paren + \
        open_paren + ZeroOrMore(data_property_expression) + close_paren + \
        close_paren

    # SameIndividual :=
    #   'SameIndividual' '(' axiomAnnotations Individual
    #                        Individual { Individual } ')'
    same_individual = \
        Word('SameIndividual') + open_paren + axiom_annotations + individual + \
        individual + ZeroOrMore(individual) + close_paren

    # DifferentIndividuals :=
    #   'DifferentIndividuals' '(' axiomAnnotations Individual Individual
    #                              { Individual } ')'
    different_individuals = \
        Word('DifferentIndividuals') + open_paren + axiom_annotations + \
        individual + individual + ZeroOrMore(individual) + close_paren

    # ClassAssertion :=
    #   'ClassAssertion' '(' axiomAnnotations ClassExpression Individual ')'
    class_assertion = \
        Word('ClassAssertion') + open_paren + axiom_annotations + \
        class_expression + individual + close_paren

    # ObjectPropertyAssertion :=
    #   'ObjectPropertyAssertion' '(' axiomAnnotations ObjectPropertyExpression
    #                                 sourceIndividual targetIndividual ')'
    object_property_assertion = \
        Word('ObjectPropertyAssertion') + open_paren + axiom_annotations + \
        object_property_expression + individual + individual + close_paren

    # NegativeObjectPropertyAssertion :=
    #   'NegativeObjectPropertyAssertion' '(' axiomAnnotations
    #                                         ObjectPropertyExpression
    #                                         sourceIndividual
    #                                         targetIndividual ')'
    negative_object_property_assertion = \
        Word('NegativeObjectPropertyAssertion') + open_paren + \
        axiom_annotations + object_property_expression + individual + \
        individual + close_paren

    # DataPropertyAssertion :=
    #   'DataPropertyAssertion' '(' axiomAnnotations DataPropertyExpression
    #                               sourceIndividual targetValue ')'
    data_property_assertion = \
        Word('DataPropertyAssertion') + open_paren + axiom_annotations + \
        data_property_expression + individual + individual + close_paren

    # NegativeDataPropertyAssertion :=
    #   'NegativeDataPropertyAssertion' '(' axiomAnnotations
    #                                       DataPropertyExpression
    #                                       sourceIndividual targetValue ')'
    negative_data_property_assertion = \
        Word('NegativeDataPropertyAssertion') + open_paren + \
        axiom_annotations + data_property_expression + individual + \
        individual + close_paren

    # Assertion :=
    #   SameIndividual | DifferentIndividuals | ClassAssertion |
    #   ObjectPropertyAssertion | NegativeObjectPropertyAssertion |
    #   DataPropertyAssertion | NegativeDataPropertyAssertion
    assertion = same_individual | different_individuals | class_assertion | \
        object_property_assertion | negative_object_property_assertion | \
        data_property_assertion | negative_data_property_assertion

    # AnnotationSubject := IRI | AnonymousIndividual
    annotation_subject = iri | anonymous_individual
    # AnnotationValue := AnonymousIndividual | IRI | Literal
    annotation_value = anonymous_individual | iri | literal

    # AnnotationAssertion :=
    #   'AnnotationAssertion' '(' axiomAnnotations AnnotationProperty
    #                             AnnotationSubject AnnotationValue ')'
    annotation_assertion = \
        Word('AnnotationAssertion') + open_paren + axiom_annotations + \
        ann_prop + annotation_subject + annotation_value + close_paren

    sub_annotation_property = ann_prop
    super_annotation_property = ann_prop
    # SubAnnotationPropertyOf :=
    #   'SubAnnotationPropertyOf' '(' axiomAnnotations subAnnotationProperty
    #                                 superAnnotationProperty ')'
    sub_annotation_property_of = \
        Word('SubAnnotationPropertyOf') + open_paren + axiom_annotations + \
        sub_annotation_property + super_annotation_property + close_paren

    # AnnotationPropertyDomain :=
    #   'AnnotationPropertyDomain' '(' axiomAnnotations AnnotationProperty
    #                                  IRI ')'
    annotation_property_domain = \
        Word('AnnotationPropertyDomain') + open_paren + axiom_annotations + \
        ann_prop + iri + close_paren

    # AnnotationPropertyRange :=
    #   'AnnotationPropertyRange' '(' axiomAnnotations AnnotationProperty
    #                                 IRI ')'
    annotation_property_range = \
        Word('AnnotationPropertyRange') + open_paren + axiom_annotations + \
        ann_prop + iri + close_paren

    # AnnotationAxiom :=
    #   AnnotationAssertion | SubAnnotationPropertyOf |
    #   AnnotationPropertyDomain | AnnotationPropertyRange
    annotation_axiom = annotation_assertion | sub_annotation_property_of | \
        annotation_property_domain | annotation_property_range

    # Axiom := Declaration | ClassAxiom | ObjectPropertyAxiom |
    #   DataPropertyAxiom | DatatypeDefinition | HasKey | Assertion |
    #   AnnotationAxiom
    axiom = declaration | class_axiom | object_property_axiom | \
        data_property_axiom | datatype_definition | has_key | assertion | \
        annotation_axiom
    comment = hash + restOfLine
    empty_line = White() + lineEnd
    axioms = ZeroOrMore(axiom | comment | empty_line)
    ontology = Word('Ontology') + open_paren + \
        Optional(ontology_iri + Optional(version_iri)) + \
        directly_imports_documents + ontology_annotations + axioms

    ontology_document = ZeroOrMore(prefix_declaration) + ontology

    def __init__(self):
        # self.ontology.addParseAction(self.dbg)
        self.object_union_of.addParseAction(self.dbg)

    def dbg(self, res):
        import pdb; pdb.set_trace()
        pass

    def parse(self, file_path):
        self.ontology_document.parseFile(file_path)