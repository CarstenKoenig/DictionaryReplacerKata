namespace DictionaryReplacerKata.Tests

open System
open Xunit
open DictionaryReplacerKata

module ``the replace method`` =

    module ``with an empty dictionary`` =
        let emptyDictionary : DictionaryReplacer.Dictionary =
                Map.empty
        let replace =
            DictionaryReplacer.replace emptyDictionary
        
        [<Fact>]
        let ``input : '' yields output ''`` () =
            // Arrange
            let input = ""
           
            // Act
            let result = replace input
            
            // Assert
            Assert.Equal("", result)
            
    module ``with an dictionary containing temp='temporary' and name='John Doe'`` =
        let dictionary : DictionaryReplacer.Dictionary =
                Map.ofList
                    [
                        ("temp", "temporary")
                        ("name", "John Doe")
                    ]
        let replace =
            DictionaryReplacer.replace dictionary
        
        [<Fact>]
        let ``input : '$temp$' yields output 'temporary'`` () =
            // Arrange
            let input = "$temp$"
           
            // Act
            let result = replace input
            
            // Assert
            Assert.Equal("temporary", result)
            
        [<Fact>]
        let ``input : '$temp$ here comes the name $name$' yields output 'temporary here comes the name John Doe'`` () =
            // Arrange
            let input = "$temp$ here comes the name $name$"
           
            // Act
            let result = replace input
            
            // Assert
            Assert.Equal("temporary here comes the name John Doe", result)
