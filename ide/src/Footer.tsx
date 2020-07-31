import React from 'react';

type FooterProps = {
  message: string;
};

export default function Footer({ message }: FooterProps) {
  return (
    <div className="footer-container">
      {message}
    </div>
  );
}
