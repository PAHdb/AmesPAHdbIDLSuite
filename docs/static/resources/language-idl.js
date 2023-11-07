Prism.languages.idl = {

    'comment': {

	pattern: /;.*/g,

	lookbehind: false
    },
   
    'keyword': /\b(if|then|begin|else|while|do|for|return|function|pro|common|new|switch|case|break|continue|end|endif|endelse|endwhile|endswitch|endcase)\b/ig,
    
    'option': [
	{
	    
	    pattern: /(\(|,\s*\$?\s*)\/\w+/g,

	    lookbehind : true
	},

	{
	    pattern: /(\(|,\s*\$?\s*)\w+\s*=\s*[\/+a-zA-z0-9\.' !\[-]+/g,

	    inside: {

		'string': /("|').*?\1/g, 
 
		'number': /\b-?(0x[\dA-Fa-f]+|\d*\.?\d+([DdEe]-?\d*)?)\b/g,

		'punctuation': /[\[\](),.\$]/g
	    },
	    
	    lookbehind: true
	}
    ],

    'string': /("|').*?\1/g, 
    
    'function': {

	pattern: /\w+\(/ig,

	inside: {

	    punctuation: /\(/
	}
    },
    
    'number': /\b-?(0x[\dA-Fa-f]+|\d*\.?\d+([DdEe]-?\d*)?)\b/g,

    'operator': /[-+]{1,2}|\bgt\b|>|\blt\b|<|\beq\b|\band\b|\bor\b|\?|:|\*|\/|~|\^|\bmod\b|!null/ig,

    'ignore': /&(lt|gt|amp);/gi,

    'punctuation': /[\[\](),.\$]/g
};
