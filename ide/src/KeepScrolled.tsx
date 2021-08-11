import React, { useEffect } from 'react';

// The number of lines is a prop to force KeepScrolled to re-render whenever the
// input changes size. There is probably definitely a better way to do this
// eslint-disable-next-line
export default function KeepScrolled({ children, numLines }: {children: any, numLines: number}) {
  const container = React.useState<React.RefObject<HTMLDivElement>>(React.createRef())[0];
  const [lastScrollTop, setLastScrollTop] = React.useState<number>(0);
  const [isUnlocked, setIsUnlocked] = React.useState<boolean>(false as false);
  // The trouble is, when we hear that the number of lines has changed, the
  // codemirror hasn't actually changed size yet. So we need to wait one render
  // before we can successfully scroll all the way down
  const [updateNextFrame, setUpdateNextFrame] = React.useState<boolean>(false as false);
  useEffect(() => {
    if (updateNextFrame) {
      const contain = container.current;
      if (!isUnlocked && contain !== null) {
        // Scroll to the end shortcut
        contain.scrollTo(0, 9999999999);
      }
      setUpdateNextFrame(false);
    } else {
      setUpdateNextFrame(true);
    }
  });
  const togetherStyle = {
    width: '40em',
    maxWidth: '70%',
    margin: '2em auto',
  };
  function onScroll() {
    const cont = container.current;
    if (cont === null) {
      throw new Error('scroll before ref registered??');
    }
    if (cont.scrollHeight - cont.scrollTop === cont.clientHeight) {
      setIsUnlocked(false);
    } else if (cont.scrollTop < lastScrollTop) {
      setIsUnlocked(true);
    }
    setLastScrollTop(cont.scrollTop);
  }
  return (
    <div
      style={{ gridRow: '1', width: '100%', overflowY: 'scroll' }}
      ref={container}
      onScroll={onScroll}
    >
      <div style={togetherStyle}>
        {children}
        <div style={{ clear: 'both' }} />
      </div>
    </div>
  );
}
