'use client';

import React, { useState } from 'react';
import CodeMirror from '@uiw/react-codemirror';
import { python } from '@codemirror/lang-python';
import { vscodeDark } from '@uiw/codemirror-theme-vscode';

interface CodeEditorProps {
  initialCode: string;
  onChange?: (value: string) => void;
  readOnly?: boolean;
  height?: string;
}

export function CodeEditor({
  initialCode,
  onChange,
  readOnly = false,
  height = '300px'
}: CodeEditorProps) {
  const [code, setCode] = useState(initialCode);

  const handleChange = (value: string) => {
    setCode(value);
    if (onChange) {
      onChange(value);
    }
  };

  return (
    <div className="border border-dark-pink/20 rounded-md overflow-hidden">
      <CodeMirror
        value={code}
        height={height}
        extensions={[python()]}
        onChange={handleChange}
        theme={vscodeDark}
        readOnly={readOnly}
        basicSetup={{
          lineNumbers: true,
          highlightActiveLineGutter: true,
          highlightSpecialChars: true,
          foldGutter: true,
          drawSelection: true,
          dropCursor: true,
          allowMultipleSelections: true,
          indentOnInput: true,
          syntaxHighlighting: true,
          bracketMatching: true,
          closeBrackets: true,
          autocompletion: true,
          rectangularSelection: true,
          crosshairCursor: true,
          highlightActiveLine: true,
          highlightSelectionMatches: true,
          closeBracketsKeymap: true,
          defaultKeymap: true,
          searchKeymap: true,
          historyKeymap: true,
          foldKeymap: true,
          completionKeymap: true,
          lintKeymap: true,
        }}
        className="text-sm font-mono"
      />
    </div>
  );
}

export default CodeEditor; 