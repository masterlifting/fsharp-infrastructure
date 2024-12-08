module Infrastructure.Parser

open Infrastructure.Domain

module Html =
    open HtmlAgilityPack

    let load (html: string) =
        try
            let document = HtmlDocument()
            document.LoadHtml html
            Ok document
        with ex ->
            Error <| NotSupported ex.Message

    let getNode (xpath: string) (html: HtmlDocument) =
        try
            match html.DocumentNode.SelectSingleNode(xpath) with
            | null -> Ok None
            | node -> Ok <| Some node
        with ex ->
            Error <| NotSupported ex.Message

    let getNodes (xpath: string) (html: HtmlDocument) =
        try
            match html.DocumentNode.SelectNodes(xpath) with
            | null -> Ok None
            | nodes -> Ok <| Some nodes
        with ex ->
            Error <| NotSupported ex.Message

    let getAttributeValue (attribute: string) (node: HtmlNode) =
        try
            match node.GetAttributeValue(attribute, "") with
            | "" -> Ok None
            | value -> Ok <| Some value
        with ex ->
            Error <| NotSupported ex.Message
